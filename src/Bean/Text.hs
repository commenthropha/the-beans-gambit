module Bean.Text where

-- Variations for the opening of the story
opening :: [String]
opening = [
            "As the sun sets on the barren wasteland, the first echoes of marching boots resonate across the horizon. The Red Army, having received word of the Blue Army's impending invasion, begins their preparations for battle. The Beans, a band of farmers and merchants, join forces with the army, bringing with them their prized cows and cannons. The atmosphere is charged with tension and anticipation as the soldiers prepare to face their greatest challenge yet. But the Beans know that they must do everything in their power to defend their homes, no matter the cost. The battlefield awaits, and with it, the hope of victory or the crushing defeat of their nation.",
            "As news of the Blue Army's planned coup on the Alexandrian border reaches the Red Army, a sense of urgency grips the soldiers. Without hesitation, the Beans - a ragtag group of farmers turned fighters - rally together with their trusty cows and cannons to defend their land. The battlefield looms ahead, and with it,the promise of chaos and carnage. But the Beans are not ones to back down from a fight, and their courage in the face of danger will be put to the ultimate test.",
            "As the Blue Army catches sight of the approaching Red Army on the horizon, a sense of dread washes over the soldiers. In a panic, they call upon the Beans - a scrappy band of farmers turned warriors - to defend their homeland from the impending threat. Without hesitation, the Beans mobilize their troops, gathering their trusty cows and cannons as they march towards the battlefield. As they near the front lines, the tension in the air is palpable, the sound of war drums pounding in their ears.",
            "As the Blue Army catches sight of the advancing Red Army in the distance, panic sets in among their troops. In a desperate bid to defend their homeland, they call upon the Beans - a scrappy group of farmers who have become seasoned warriors - to join the fight. With their cows and cannons in tow, the Beans march towards the battlefield with a fierce determination. As they draw closer, the tension in the air becomes almost unbearable, the rumble of marching feet and the clatter of weapons filling the silence. But the Beans refuse to back down, their unwavering courage and steely resolve set to be tested in the brutal and unpredictable chaos of war."
          ]

-- Story Text for when the Game Is Drawn
draw :: [String]
draw =  [
          "The Red and Blue armies clashed on the battlefield, their swords clanging and shields clashing as they fought for supremacy. The battle raged on for hours, with neither side gaining an advantage over the other. Just when it semed like one side would emerge victorious, the other would rally and push them back. The soldiers were exhausted, but they fought on, unwilling to give up. As the sun began to set, the two armies found themselves back where they had started, facing each other across the field. It was then that they realized the futility of their struggle, and they called for a truce. The battle ended in a draw, and the soldiers returned home, knowing that they had fought bravely, but ultimately achieved nothing.",
          "The Red army and the Blue army clashed on the battlefield in a battle that lasted for hours. The fighting was fierce, and both sides suffered heavy casualties. However, as the sun began to set, it became clear that neither side had gained a significant advantage. It felt like the battle was going in circles - all the soldiers felt like they had been in the exact same situation before, with both armies making gains only to lose them again. Finally, as darkness fell, the commanders on both sides decided to call a truce and withdraw their troops. The battle ended in a draw."
        ]

--------------------------------------------------------------------------------
-- | Red Player

-- Story Text for when the Red Player is Winning & Moves A Cow
redWCMove :: [String]
redWCMove = [
              "The Red Army, with a clear advantage, mobilizes a cow to further solidify their position on the battlefield. Faced with mounting pressure, the Blue Army realizes that their upcoming move will be a critical one. One misstep could lead to the end of their aspirations, underscoring the importance of careful planning and precise execution.",
              "As the cow makes its way through the Red Army's ranks, the soldiers skillfully maneuver to create a stronger and more impenetrable line of defense. The formidable presence of the cow bolsters the Red Army's position on the battlefield, instilling a sense of confidence and assurance among the troops. With each passing moment, the Red Army's grip on the battlefield tightens, making it increasingly challenging for the Blue Army to mount any meaningful offense.",
              "With the Red Army holding a commanding lead, they move a cow to fortify their already formidable position on the battlefield. The Blue Army is acutely aware of the mounting pressure and understands that their next move could be a make-or-break moment. A single misstep could spell the end of their mission, emphasizing the significance of meticulous planning and flawless execution.",
              "The Red Army recognizes the significance of maintaining the upper hand and swiftly takes action by dispatching a cow to a strategic location on the battlefield. As the cow moves into position, the soldiers deftly adjust their formation, creating an even more impenetrable wall of defense. The commanding presence of the cow sends a clear message to the Blue Army, underscoring the Red Army's dominance on the battlefield. With each passing moment, the Red Army continues to tighten its grip on the battlefield, making it increasingly difficult for the Blue Army to gain any traction."
            ]

-- Story Text for when the Red Player is Equal to the Blue Player & Moves A Cow
redECMove :: [String]
redECMove = [
              "The battle had been fiercely contested, with neither army able to gain a clear advantage over the other. However, as the Red Army strategically moves a cow to a different flank, a sense of uncertainty begins to creep in among the Blue Army. The cow's imposing presence on the battlefield casts a shadow over the Blue Army's morale, and their confidence begins to wane.",
              "As the battle rages on, both armies struggle to gain the upper hand. However, the Red Army's decisive move to position a cow in a vulnerable area sends shockwaves through the Blue Army's ranks. The sight of the massive cow looming over them sends a chill down their spines, and the Blue Army's morale begins to falter.",
              "With the Red Army moving a cow to a different position, the tides of the conflict begin to shift. The cow's thunderous footsteps and intimidating stature cast a pall over the battlefield, serving as a stark reminder of the formidable opponent they are up against.",
              "As the Red Army maneuvers a cow to a strategic location, the dynamics of the battlefield begin to change. The once evenly matched armies now find themselves on uneven ground, with the Red Army gaining an upper hand. The imposing presence of the cow serves as a constant reminder of the precariousness of the Blue Army's position. Fear and uncertainty spread among their ranks, and the soldiers begin to question their ability to emerge victorious."
            ]

-- Story Text for when the Red Player is Losing & Moves A Cow
redLCMove :: [String]
redLCMove = [
              "With the Blue Army making significant strides, the Red Army finds themselves on the back foot, desperately searching for a way to turn the tide of the battle. After careful consideration, they decide to move one of their cows to a new location on the battlefield. This decision is met with skepticism and uncertainty from some of thir soldiers, who fear that it may expose their flank to attack. However, the cow's imposing presence on the battlefield inspires a newfound sense of confidence among the Red Army's ranks.",
              "The Red Army found themselves struggling to keep up with the relentless assault from the Blue Army. With their defenses crumbling and their troops in disarray, they knew they needed to act fast if they wanted to avoid defeat. In a bold move, they decided to move one of their cows to the front lines, hoping that its sheer size and presence would be enough to intimidate the enemy and turn the tide of the battle in their favor.",
              "The Red Army has been struggling to keep up with the relentless assault of the Blue Army, losing ground with each passing minute. In a desperate bid to regain their footing, they have decided to try moving a cow elsewhere. Despite the confusion and uncertainty among the troops, they trust their leaders and hope that this unconventional move will help them turn the tide of the battle. Only time will tell if this maneuver will be enough to help the Red Army emerge victorious.",
              "The Red Army had been outnumbered and outmatched by the Blue Army's superior tactics. In a bold move, they decided to deploy a cow to a different position in the battlefield. Some soldiers were skeptical, but the commanding officer insisted on the cow's strategic importance. As the cow marched onto the battlefield, the Red Army watched with bated breath, hoping that their unorthodox plan would work. The cow's sudden appearance seemed to catch the Blue Army off guard, and the Red Army began to push back with renewed energy."
            ]

-- Story Text for when the Red Player is Winning & Moves A Bean
redWBMove :: [String]
redWBMove = [
              "A Red Bean soldier dashed across the battlefield, dodging enemy fire and weaving through the chaos of war. As they ran, their comrades cheered them on, knowing that this was a critical moment in the battle. The Red Army were on the front foot, however they were struggling to keep up with the Blue Army's recent assault. But now, with victory in sight, they were determined to push forward and end the conflict once and for all.",
              "The Red Army had been fighting hard, pushing back against the relentless onslaught of the Blue Army. They had managed to gain the upper hand, but they knew that their victory was far from secure. The longer the battle dragged on, the greater the risk of the Blue Army regrouping and launching a counter-attack. That's when they hatched a plan - they would send a single Red Bean to take control of a key position on the battlefield. As he clambered across the battlefield, a blanket of uncertainty fell upon the Blue Army's troops; what was the intention behind this sudden change? Only time would tell..",
              "The Red Army, despite having the upper hand in the battle, knew that they could not afford to drag it out for long. They needed to end the conflict as soon as possible to avoid any unnecessary losses of life and resources. In the midst of this chaos, a single Bean stood out. With determination etched on his face, he bravely dashed through the onslaught of the enemy attack to reach a different position; he knew that the success of the battle rested on his shoulders, and he was willing to risk his life to help his comrades.",
              "Already with the upper hand, the Red army remained resolute in their victory. Directed by the officials, a Red Bean dashed through the enemy attack to reach a more advantageous position in the battlefield - navigating through cannonfire and the minefields of cow faeces. As the Red Bean sprinted towards his objective, he could hear cannonballs whizzing past his head and explosions shaking the ground beneath him. Despite the chaos around him, the Red Bean remained focused on his mission, knowing that the fate of the battle may very well rest on his success."
            ]

-- Story Text for when the Red Player is Equal to the Blue Player & Moves A Bean
redEBMove :: [String]
redEBMove = [
              "The battle had been at a stalemate for hours. As the combat raged on, the Red Army decided to push for an advantage by sending a Red Bean to a different position. With the battle raging all around him, he sprinted towards his objective, dodging enemy fire and ducking behind cover. The enemy troops were caught off guard by this sudden move, and an air of uncertainty rose among the soldiers of the Blue army.",
              "Both sides continued to be locked in a fierce struggle for dominance. Despite their best efforts, neither side could gain a decisive advantage, and the conflict had descended into a grueling war of attrition. Sensing the need to change tactics, the Red Army made a bold move. They dispatched a skilled Red Bean to infiltrate the enemy lines and seize a strategic position. With the fate of the battle hanging in the balance, the Red Bean set out on his mission, determined to turn the tide of the war in his army's favor.",
              "The battle between the Red and Blue Armies had reached a stalemate. Neither side had been able to gain a significant advantage, and both armies were on equal ground. The Red Army knew that they needed to take action to break the impasse. They decided to send one of their most skilled Beans to a different position. Despite the odds stacked against him, the Red Bean remained determined to reach his destination in an effort to help his comrades gain the upper hand in the battle.",
              "The battle between the Red Army and the Blue Army had been raging on with neither side gaining the upper hand. The Red Army's commanders recognized the need to shift the balance in their favor and decided to send one of their Red Beans to a different position. This move was meant to outflank the enemy and create a new front for the Red Army to attack from. The Red Bean, armed with nothing but his blade and his bravery, set off towards the enemy lines, dodging cannon fire and explosions along the way. As he reached his new position, he could see the enemy's defenses weakened, giving the Red Army an opening to exploit."
            ]

-- Story Text for when the Red Player is Losing & Moves A Bean
redLBMove :: [String]
redLBMove = [
              "With their backs against the wall, the Red Army leaders knew they had to try something bold to turn the tide of the battle. They selected a brave Bean to carry out a desperate mission: to infiltrate the enemy's ranks and sow chaos from within. With no time to waste, the Red Bean made his way to a different position in the battlefield, hoping to catch the enemy off guard. Though the odds were stacked against him, the Red Bean remained determined, fueled by the hope that his actions could make the difference between victory and defeat.",
              "In the onslaught between the Red and Blue armies, the former remained on the back foot. With their backs against the wall, the Red Army leaders knew they had to try something bold to turn the tide of the battle. They selected a brave Bean to carry out a desperate mission: to infiltrate the enemy's ranks and sow chaos from within. With no time to waste, the Red Bean made his way to a different position in the battlefield, hoping to catch the enemy off guard. Though the odds were stacked against him, the Red Bean remained determined, fueled by the hope that his actions could make the difference between victory and defeat.",
              "The Blue Army was edging towards victory and looked to be on the brink of breaking through the Red Army's defences. The Red Army commanders knew they needed to act quickly to avoid a complete rout. They ordered a Red Bean to move to a different position in the battlefield to try and turn the tide. The Bean, determined to save his comrades, braved the enemy onslaught and managed to reach the new position. From there, the Red Army hoped to launch a surprise attack on the Blue Army and start to turn the tide of the battle back in their favour.",
              "In a last-ditch effort to turn the tide of the battle, the Red Army commanders sent a Bean soldier to a different position on the battlefield. The Red Bean's mission was to disrupt the enemy's supply lines and create chaos behind their lines. The Red Bean was well aware of the danger of his mission and the odds against him, but he remained determined to give it his all; he managed to sneak behind the enemy's lines undetected, causing havoc among the enemy's ranks. His actions bought valuable time for the Red Army to regroup."
            ]

-- Story Text for when a Red Bean captures a Blue Bean
redCapture :: [String]
redCapture =  [
                "The Red Army senses the shift in momentum and seizes the opportunity to press their advantage, launching a fierce assault on the weakened flank. A nameless Red Bean, fuelled by the flames of vengeance, saw an opportunity and seized it. Amidst the chaos, he made his way behind a Blue Bean and - without giving him any chance to react - thrusted his blade between his ribcage.",
                "A lone Red Bean emerged from the fray, his eyes ablaze with determination. Spotting a Blue Bean in his sights, he swiftly drew his sword and charged towards his foe. The Blue Bean turned to face his attacker, but it was too late. The Red Bean's blade flashed in the sunlight as it plunged into his opponent's chest, piercing his heart. For the Red Army, the Blue Bean's cry of anguish was a victory fanfare; for the Blue Army, however, it was a warning.",
                "The battle raged on as the Red Army continued to gain ground. In the midst of the conflict, a Red Bean warrior spotted a Blue Bean officer, who appeared to be rallying his troops. The Red Bean saw an opportunity to strike a critical blow against the enemy's morale. Without hesitation, he charged towards the Blue Bean and delivered a swift, deadly strike. As the blood of the deceased sept into the crevasses of the ground, the Blue Army soldiers immediately wavered: their resolve shaken, and their ranks broken.",
                "As the battle raged on, the tension between the Red and Blue armies reached a boiling point. In the heat of the moment, a Red Bean warrior and a Blue Bean locked eyes. Both combatants knew that only one of them would emerge victorious. The Red Bean made the first move, launching a vicious attack that caught the Blue Bean off guard. In a flash, the Red Bean's sword sliced through the Blue Bean's defenses, sending him crashing to the ground."
              ]

-- Story Text for when the Red Player wins
redWin :: [String]
redWin =  [
            "It's the Blue Army's turn to make a move but what's this? It appears that their cows have been surrounded. The Blue Army makes every effort to break free from the trap, but it becomes aparent that they are trapped with no escape route in sight.. Defeated, despaired and distraught - the Blue Army yields; resigned to their fate as they succumb to the overwhelming feeling of hopelesness in defeat.",
            "The Blue Army has reached its turn to advance, yet an unexpected obstacle presents itself. Their cows have been encircled, and despite any attempts to break free, it seems the Blue Army has been cornered. Defeated and disheartened, the Blue Army concedes.",
            "The Blue Army's turn to make a move has come, but it seems that their cows have been encircled. Despite their attempts to break through the blockade, it appears that there is no way out for the Blue Army. With defeat looming over them and hopelessness setting in, the Blue Army surrenders.",
            "As it comes time for the Blue Army to make their move, they are faced with a dire situation - their cows have been surrounded. Despite their valiant efforts to escape the trap, the Blue Army finds themselves at a dead end with no way out. With no other options left, they concede defeat, overwhelmed by a sense of despair and hopelessness."
          ]

--------------------------------------------------------------------------------
-- | Blue Player

-- Story Text for when the Blue Player is Winning & Moves A Cow
blueWCMove :: [String]
blueWCMove =  [
                "The Blue Army, with a clear advantage, mobilizes a cow to further solidify their position on the battlefield. Faced with mounting pressure, the Red Army realizes that their upcoming move will be a critical one. One misstep could lead to the end of their aspirations, underscoring the importance of careful planning and precise execution.",
                "As the cow makes its way through the Blue Army's ranks, the soldiers skillfully maneuver to create a stronger and more impenetrable line of defense. The formidable presence of the cow bolsters the Blue Army's position on the battlefield, instilling a sense of confidence and assurance among the troops. With each passing moment, the Blue Army's grip on the battlefield tightens, making it increasingly challenging for the Red Army to mount any meaningful offense.",
                "With the Blue Army holding a commanding lead, they move a cow to fortify their already formidable position on the battlefield. The Red Army is acutely aware of the mounting pressure and understands that their next move could be a make-or-break moment. A single misstep could spell the end of their mission, emphasizing the significance of meticulous planning and flawless execution.",
                "The Blue Army recognizes the significance of maintaining the upper hand and swiftly takes action by dispatching a cow to a strategic location on the battlefield. As the cow moves into position, the soldiers deftly adjust their formation, creating an even more impenetrable wall of defense. The commanding presence of the cow sends a clear message to the Red Army, underscoring the Blue Army's dominance on the battlefield. With each passing moment, the Blue Army continues to tighten its grip on the battlefield, making it increasingly difficult for the R Army to gain any traction."
              ]

-- Story Text for when the Blue Player is Equal to the Red Player & Moves A Cow
blueECMove :: [String]
blueECMove =  [
                "The battle had been fiercely contested, with neither army able to gain a clear advantage over the other. However, as the Blue Army strategically moves a cow to a different flank, a sense of uncertainty begins to creep in among the Red Army. The cow's imposing presence on the battlefield casts a shadow over the Red Army's morale, and their confidence begins to wane.",
                "As the battle rages on, both armies struggle to gain the upper hand. However, the Blue Army's decisive move to position a cow in a vulnerable area sends shockwaves through the Red Army's ranks. The sight of the massive cow looming over them sends a chill down their spines, and the Red Army's morale begins to falter.",
                "With the Blue Army moving a cow to a different position, the tides of the conflict begin to shift. The cow's thunderous footsteps and intimidating stature cast a pall over the battlefield, serving as a stark reminder of the formidable opponent they are up against.",
                "As the Blue Army maneuvers a cow to a strategic location, the dynamics of the battlefield begin to change. The once evenly matched armies now find themselves on uneven ground, with the Blue Army gaining an upper hand. The imposing presence of the cow serves as a constant reminder of the precariousness of the Red Army's position. Fear and uncertainty spread among their ranks, and the soldiers begin to question their ability to emerge victorious."
              ]

-- Story Text for when the Blue Player is Losing & Moves A Cow
blueLCMove :: [String]
blueLCMove =  [
                "With the Blue Army making significant strides, the Red Army finds themselves on the back foot, desperately searching for a way to turn the tide of the battle. After careful consideration, they decide to move one of their cows to a new location on the battlefield. This decision is met with skepticism and uncertainty from some of thir soldiers, who fear that it may expose their flank to attack. However, the cow's imposing presence on the battlefield inspires a newfound sense of confidence among the Red Army's ranks.",
                "The Red Army found themselves struggling to keep up with the relentless assault from the Blue Army. With their defenses crumbling and their troops in disarray, they knew they needed to act fast if they wanted to avoid defeat. In a bold move, they decided to move one of their cows to the front lines, hoping that its sheer size and presence would be enough to intimidate the enemy and turn the tide of the battle in their favor.",
                "The Red Army has been struggling to keep up with the relentless assault of the Blue Army, losing ground with each passing minute. In a desperate bid to regain their footing, they have decided to try moving a cow elsewhere. Despite the confusion and uncertainty among the troops, they trust their leaders and hope that this unconventional move will help them turn the tide of the battle. Only time will tell if this maneuver will be enough to help the Red Army emerge victorious.",
                "The Red Army had been outnumbered and outmatched by the Blue Army's superior tactics. In a bold move, they decided to deploy a cow to a different position in the battlefield. Some soldiers were skeptical, but the commanding officer insisted on the cow's strategic importance. As the cow marched onto the battlefield, the Red Army watched with bated breath, hoping that their unorthodox plan would work. The cow's sudden appearance seemed to catch the Blue Army off guard, and the Red Army began to push back with renewed energy."
              ]

-- Story Text for when the Blue Player is Winning & Moves A Bean
blueWBMove :: [String]
blueWBMove =  [
                "A Blue Bean soldier dashed across the battlefield, dodging enemy fire and weaving through the chaos of war. As they ran, their comrades cheered them on, knowing that this was a critical moment in the battle. The Blue Army were on the front foot, however they were struggling to keep up with the Red Army's recent assault. But now, with victory in sight, they were determined to push forward and end the conflict once and for all.",
                "The Blue Army had been fighting hard, pushing back against the relentless onslaught of the Red Army. They had managed to gain the upper hand, but they knew that their victory was far from secure. The longer the battle dragged on, the greater the risk of the Red Army regrouping and launching a counter-attack. That's when they hatched a plan - they would send a single Blue Bean to take control of a key position on the battlefield. As he clambered across the battlefield, a blanket of uncertainty fell upon the Red Army's troops; what was the intention behind this sudden change? Only time would tell..",
                "The Blue Army, despite having the upper hand in the battle, knew that they could not afford to drag it out for long. They needed to end the conflict as soon as possible to avoid any unnecessary losses of life and resources. In the midst of this chaos, a single Bean stood out. With determination etched on his face, he bravely dashed through the onslaught of the enemy attack to reach a different position; he knew that the success of the battle rested on his shoulders, and he was willing to risk his life to help his comrades.",
                "Already with the upper hand, the Blue army remained resolute in their victory. Directed by the officials, a Blue Bean dashed through the enemy attack to reach a more advantageous position in the battlefield - navigating through cannonfire and the minefields of cow faeces. As the Blue Bean sprinted towards his objective, he could hear cannonballs whizzing past his head and explosions shaking the ground beneath him. Despite the chaos around him, the Blue Bean remained focused on his mission, knowing that the fate of the battle may very well rest on his success."
              ]
-- Story Text for when the Blue Player is Equal to the Red Player & Moves A Bean
blueEBMove :: [String]
blueEBMove =  [
                "The battle had been at a stalemate for hours. As the combat raged on, the Blue Army decided to push for an advantage by sending a Blue Bean to a different position. With the battle raging all around him, he sprinted towards his objective, dodging enemy fire and ducking behind cover. The enemy troops were caught off guard by this sudden move, and an air of uncertainty rose among the soldiers of the Red army.",
                "Both sides continued to be locked in a fierce struggle for dominance. Despite their best efforts, neither side could gain a decisive advantage, and the conflict had descended into a grueling war of attrition. Sensing the need to change tactics, the Blue Army made a bold move. They dispatched a skilled Blue Bean to infiltrate the enemy lines and seize a strategic position. With the fate of the battle hanging in the balance, the Blue Bean set out on his mission, determined to turn the tide of the war in his army's favor.",
                "The battle between the Red and Blue Armies had reached a stalemate. Neither side had been able to gain a significant advantage, and both armies were on equal ground. The Blue Army knew that they needed to take action to break the impasse. They decided to send one of their most skilled Beans to a different position. Despite the odds stacked against him, the Blue Bean remained determined to reach his destination in an effort to help his comrades gain the upper hand in the battle.",
                "The battle between the Red Army and the Blue Army had been raging on with neither side gaining the upper hand. The Blue Army's commanders recognized the need to shift the balance in their favor and decided to send one of their Blue Beans to a different position. This move was meant to outflank the enemy and create a new front for the Blue Army to attack from. The Blue Bean, armed with nothing but his blade and his bravery, set off towards the enemy lines, dodging cannon fire and explosions along the way. As he reached his new position, he could see the enemy's defenses weakened, giving the Blue Army an opening to exploit."
              ]

-- Story Text for when the Blue Player is Losing & Moves A Bean
blueLBMove :: [String]
blueLBMove =  [
                "With their backs against the wall, the Blue Army leaders knew they had to try something bold to turn the tide of the battle. They selected a brave Bean to carry out a desperate mission: to infiltrate the enemy's ranks and sow chaos from within. With no time to waste, the Blue Bean made his way to a different position in the battlefield, hoping to catch the enemy off guard. Though the odds were stacked against him, the Blue Bean remained determined, fueled by the hope that his actions could make the difference between victory and defeat.",
                "In the onslaught between the Red and Blue armies, the latter remained on the back foot. With their backs against the wall, the Blue Army leaders knew they had to try something bold to turn the tide of the battle. They selected a brave Bean to carry out a desperate mission: to infiltrate the enemy's ranks and sow chaos from within. With no time to waste, the Blue Bean made his way to a different position in the battlefield, hoping to catch the enemy off guard. Though the odds were stacked against him, the Blue Bean remained determined, fueled by the hope that his actions could make the difference between victory and defeat.",
                "The Red Army was edging towards victory and looked to be on the brink of breaking through the Blue Army's defences. The Blue Army commanders knew they needed to act quickly to avoid a complete rout. They ordered a Blue Bean to move to a different position in the battlefield to try and turn the tide. The Bean, determined to save his comrades, braved the enemy onslaught and managed to reach the new position. From there, the Blue Army hoped to launch a surprise attack on the Red Army and start to turn the tide of the battle back in their favour.",
                "In a last-ditch effort to turn the tide of the battle, the Blue Army commanders sent a Bean to a different position on the battlefield. The Blue Bean's mission was to disrupt the enemy's supply lines and create chaos behind their lines. The Blue Bean was well aware of the danger of his mission and the odds against him, but he remained determined to give it his all; he managed to sneak behind the enemy's lines undetected, causing havoc among the enemy's ranks. His actions bought valuable time for the Blue Army to regroup."
              ]

-- Story Text for when a Blue Bean captures a Red Bean
blueCapture :: [String]
blueCapture = [
                "The Blue Army senses the shift in momentum and seizes the opportunity to press their advantage, launching a fierce assault on the weakened flank. A nameless Blue Bean, fuelled by the flames of vengeance, saw an opportunity and seized it. Amidst the chaos, he made his way behind a Red Bean and - without giving him any chance to react - thrusted his blade between his ribcage.",
                "A lone Blue Bean emerged from the fray, his eyes ablaze with determination. Spotting a Red Bean in his sights, he swiftly drew his sword and charged towards his foe. The Red Bean turned to face his attacker, but it was too late. The Blue Bean's blade flashed in the sunlight as it plunged into his opponent's chest, piercing his heart. For the Blue Army, the Red Bean's cry of anguish was a victory fanfare; for the Red Army, however, it was a warning.",
                "The battle raged on as the Blue Army continued to gain ground. In the midst of the conflict, a Blue Bean warrior spotted a Red Bean officer, who appeared to be rallying his troops. The Blue Bean saw an opportunity to strike a critical blow against the enemy's morale. Without hesitation, he charged towards the Red Bean and delivered a swift, deadly strike. As the blood of the deceased sept into the crevasses of the ground, the Red Army soldiers immediately wavered: their resolve shaken, and their ranks broken.",
                "As the battle raged on, the tension between the Red and Blue armies reached a boiling point. In the heat of the moment, a Red Bean warrior and a Blue Bean locked eyes. Both combatants knew that only one of them would emerge victorious. The Blue Bean made the first move, launching a vicious attack that caught the Red Bean off guard. In a flash, the Blue Bean's sword sliced through the Red Bean's defenses, sending him crashing to the ground."
              ]

-- Story Text for when the Blue Player wins
blueWin :: [String]
blueWin = [
            "It's the Red Army's turn to make a move but what's this? It appears that their cows have been surrounded. The Red Army makes every effort to break free from the trap, but it becomes aparent that they are trapped with no escape route in sight.. Defeated, despaired and distraught - the Red Army yields; resigned to their fate as they succumb to the overwhelming feeling of hopelesness in defeat.",
            "The Red Army has reached its turn to advance, yet an unexpected obstacle presents itself. Their cows have been encircled, and despite any attempts to break free, it seems the Red Army has been cornered. Defeated and disheartened, the Red Army concedes.",
            "The Red Army's turn to make a move has come, but it seems that their cows have been encircled. Despite their attempts to break through the blockade, it appears that there is no way out for the Red Army. With defeat looming over them and hopelessness setting in, the Red Army surrenders.",
            "As it comes time for the Red Army to make their move, they are faced with a dire situation - their cows have been surrounded. Despite their valiant efforts to escape the trap, the Red Army finds themselves at a dead end with no way out. With no other options left, they concede defeat, overwhelmed by a sense of despair and hopelessness."
          ]