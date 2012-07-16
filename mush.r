rooms<<-read.csv("rooms.csv",stringsAsFactors=F)
paths<<-read.csv("paths.csv",stringsAsFactors=F)
paths<<-data.frame(from=1,to=2)
monsters<<-data.frame(ID=1,Name="Ghost of Quonsar",Desc="A large bald white man.",HX=0,HY=0,HZ=0,room=1)

rooms$ID<<-as.numeric(rooms$ID)	
rooms$X<<-as.numeric(rooms$X)
rooms$Y<<-as.numeric(rooms$Y)
rooms$Z<<-as.numeric(rooms$Z)
paths$from<<-as.numeric(paths$from)
paths$to<<-as.numeric(paths$to)


dir.vec<<-c("N","S","E","W","U","D")
dir.frame<<-data.frame(dir=dir.vec,X=c(1,-1,0,0,0,0),Y=c(0,0,1,-1,0,0),Z=c(0,0,0,0,1,-1),stringsAsFactors=F)

fix.tables=function(){
	rooms$ID<<-as.numeric(rooms$ID)
	rooms$X<<-as.numeric(rooms$X)
	rooms$Y<<-as.numeric(rooms$Y)
	rooms$Z<<-as.numeric(rooms$Z)
	paths$from<<-as.numeric(paths$from)
	paths$to<<-as.numeric(paths$to)
}

norm.paths=function(paths)
{
	p1=paths
	p2=data.frame(from=p1[,2],to=p1[,1])
	new=rbind(p1,p2)
	new=unique(new)
	new
	}
	
test.exit=function(from.room,to.room){
	g=subset(paths,from==from.room & to==to.room)
	if(length(g$from)==0){
		output=F
	}else{
		output=T
	}
	output
}

test.exists.coordinates=function(Xa,Ya,Za){
	new.room=subset(rooms,X==Xa & Y==Ya & Z==Za)
	if(length(new.room$Name)==0){
		output=0
	}else{
		output=new.room$ID[1]
	}
	output
}
	
position.from.id=function(here){
	current=subset(rooms,ID==here)
	current.pos=c(current[1,1:3])
	current.pos
	}

change.vector=function(direction){
	change.pos=as.vector(subset(dir.frame,dir.frame$dir==direction)[2:4])
	change.pos
}

look.other=function(here,direction){
	current.pos=position.from.id(here)
	change.pos=change.vector(direction)
	newpos = as.numeric(change.pos+as.numeric(current.pos))
	room.test=test.exists.coordinates(X=newpos[1],Y=newpos[2],Z=newpos[3])
	if(room.test>0){
		show.room(as.numeric(room.test))
	}else{
		out_text("You see a wall.")
	}
}

make.passage=function(here,new){
	vec=c(here,new)
	paths<<-rbind(paths,vec)
	paths<<-norm.paths(paths)
}
make=function(here,direction){
	current.pos=position.from.id(here)
	change.pos=change.vector(direction)
	newpos = as.numeric(change.pos+as.numeric(current.pos))
	room.test=test.exists.coordinates(X=newpos[1],Y=newpos[2],Z=newpos[3])
	if(room.test>0 & test.exit(here,room.test)){
		out_text("Room and passage already exist.")
		here
		return
	}
	if(room.test>0 & test.exit(here,room.test)==F){
		out_text("Room exists. Making passage.")
		make.passage(here,room.test)
		here
		return
	}
	if(room.test==0){
		out_text("Enter room name:")
		room_name=readline()
		out_text("Enter description:")
		description=readline()
		X=newpos[1]
		Y=newpos[2]
		Z=newpos[3]
		ID=max(rooms$ID)+1
		vec=c(X,Y,Z,room_name,description,ID)
		rooms<<-rbind(rooms,vec)
		make.passage(here,ID)
		out_text(paste("Room created: ",room_name,sep=""))
	}
	here
}
move.monsters=function(){
	for(line in 1:length(monsters$ID)){
		if(runif(1)>0.2){
			next
		}
		test.dir=sample(dir.vec,1)
		t.pos=as.numeric(position.from.id(monsters$room[line]))
		X=t.pos[1]
		Y=t.pos[2]
		Z=t.pos[3]
		HX=monsters$HX[line]
		HY=monsters$HY[line]
		HZ=monsters$HZ[line]
		current.pos=c(X,Y,Z)
		change.pos=change.vector(test.dir)
		newpos = as.numeric(change.pos+as.numeric(current.pos))
		delta=as.numeric(newpos-c(HX,HY,HZ))
		if(sum(abs(delta)>20)){
			next
		}
		test=test.exists.coordinates(newpos[1],newpos[2],newpos[3])
		if(test==0){
			next
		}
		current=test.exists.coordinates(X,Y,Z)
		if(test.exit(test,current)==F){
			next
		}
		monsters$room[line]<<-test	
		out_text(paste("Monster moved to ",test,sep=""))			
	}
}

move=function(here,direction){
	current.pos=position.from.id(here)
	change.pos=change.vector(direction)
	newpos = as.numeric(change.pos+as.numeric(current.pos))
	room.test=test.exists.coordinates(X=newpos[1],Y=newpos[2],Z=newpos[3])
	output=here
	skip=0
	if(room.test>0){
		room.to=room.test
	}else{
		out_text("Can't move that way!")
		skip=1
	}
	if(output!=0 & skip==0){
		can.exit = test.exit(here,room.to)
		if(can.exit==T){
			output=room.to
		}else{
			out_text("Can't move that way!")
			}
	}
	output
}

location.id=function(here){
	thisrec=subset(rooms,ID==here)
	c(thisrec$Name[1],thisrec$Desc[1])
}

out_text=function(text){
	cat(paste(text,"\n",sep=""))
}

show.room=function(position){
	out_text(location.id(position)[1])
	out_text(location.id(position)[2])
	monsters.here=subset(monsters,room==position)
	if(length(monsters.here$room)>0){
		for(line in 1:length(monsters.here$room)){
		out_text(paste("You see a ",monsters.here$Name[line]," here.",sep=""))
		}
	}
}

cleanup=function(){
	paths<<-norm.paths(paths)
	fix.tables()
}

parse=function(t){
	ut=toupper(t)	
	# Parse atomic direction
	if(ut == "MAP"){
		map(position)
	}
	
	if(ut %in% dir.vec){
		position<<-move(position,ut)
		show.room(position)
	}
	
	# Parse "move"command
	if(ut %in% paste("MOVE",dir.vec,sep=" ")){
		ut=strsplit(t,"\ ")[[1]][2]
		position<<-move(position,ut)
		show.room(position)
	}
	
	# Parse "make" comment
		if(ut %in% paste("MAKE",dir.vec,sep=" ")){
		ut=strsplit(ut,"\ ")[[1]][2]
		position<<-make(position,ut) #####
	}
	
	# Parse "look" coomand
	if(ut == "LOOK"){
		show.room(position)
	}
	
	# Parse "look *" comment
	if(ut %in% paste("LOOK",dir.vec,sep=" ")){
		ut=strsplit(ut,"\ ")[[1]][2]
		look.other(position,ut)  #####
	}
}

map=function(here){
	pos=as.numeric(position.from.id(here))
	X.here=pos[1]
	Y.here=pos[2]
	g=subset(rooms,Z==pos[3])
	quartz("Map",4,4)
	par(mai=c(.2,.2,.2,.2))
	plot(g$X ~ g$Y,xlim=c(X.here-20,X.here+20),ylim=c(Y.here-20,Y.here+20),xlab="",ylab="",cex=0.5,xaxt="n",yaxt="n")
	points(X.here~Y.here)
	for(line in 1:length(paths$from)){
		start=paths$from[line]
		start.pos=as.numeric(position.from.id(start))
		start.X=start.pos[1]
		start.Y=start.pos[2]
		end=paths$to[line]
		end.pos=as.numeric(position.from.id(end))
		end.X=end.pos[1]
		end.Y=end.pos[2]
		Xs=c(start.X,end.X)
		Ys=c(start.Y,end.Y)
		lines(Xs~Ys)	
	}
}

position<<-1
while(1==1){
	cleanup()
	cat("-------")
	parse(readline())
	move.monsters()
	write.csv(rooms,"rooms.csv")
	write.csv(paths,"paths.csv")
}