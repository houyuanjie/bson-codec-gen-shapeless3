case class Pet(name: String, age: Int)
case class Student(no: Long, name: String, pet: Pet)
case class ClassRoom(number: String, cleaner: Student)

val cat = Pet(name = "kitty", age = 3)
val person = Student(no = 201901060116L, name = "xiao ming", pet = cat)
val room = ClassRoom(number = "A123", cleaner = person)

import Encoder.*

val catBson = cat.toBson
val personBson = person.toBson
val roomBson = room.toBson

println(catBson)
println(personBson)
println(roomBson)

import Decoder.*

val cat2 = catBson.toOption[Pet]
val person2 = personBson.toOption[Student]
val room2 = roomBson.toOption[ClassRoom]

println(cat2)
println(person2)
println(room2)

println(cat2.contains(cat))
println(person2.contains(person))
println(room2.contains(room))

val error = room.toBson.toOption[Pet]
println(error)
