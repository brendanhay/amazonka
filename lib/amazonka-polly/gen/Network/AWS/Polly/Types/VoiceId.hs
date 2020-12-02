{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.VoiceId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.VoiceId where

import Network.AWS.Prelude

data VoiceId
  = Aditi
  | Amy
  | Astrid
  | Bianca
  | Brian
  | Camila
  | Carla
  | Carmen
  | Celine
  | Chantal
  | Conchita
  | Cristiano
  | Dora
  | Emma
  | Enrique
  | Ewa
  | Filiz
  | Geraint
  | Giorgio
  | Gwyneth
  | Hans
  | Ines
  | Ivy
  | Jacek
  | Jan
  | Joanna
  | Joey
  | Justin
  | Karl
  | Kendra
  | Kevin
  | Kimberly
  | Lea
  | Liv
  | Lotte
  | Lucia
  | Lupe
  | Mads
  | Maja
  | Marlene
  | Mathieu
  | Matthew
  | Maxim
  | Mia
  | Miguel
  | Mizuki
  | Naja
  | Nicole
  | Olivia
  | Penelope
  | Raveena
  | Ricardo
  | Ruben
  | Russell
  | Salli
  | Seoyeon
  | Takumi
  | Tatyana
  | Vicki
  | Vitoria
  | Zeina
  | Zhiyu
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText VoiceId where
  parser =
    takeLowerText >>= \case
      "aditi" -> pure Aditi
      "amy" -> pure Amy
      "astrid" -> pure Astrid
      "bianca" -> pure Bianca
      "brian" -> pure Brian
      "camila" -> pure Camila
      "carla" -> pure Carla
      "carmen" -> pure Carmen
      "celine" -> pure Celine
      "chantal" -> pure Chantal
      "conchita" -> pure Conchita
      "cristiano" -> pure Cristiano
      "dora" -> pure Dora
      "emma" -> pure Emma
      "enrique" -> pure Enrique
      "ewa" -> pure Ewa
      "filiz" -> pure Filiz
      "geraint" -> pure Geraint
      "giorgio" -> pure Giorgio
      "gwyneth" -> pure Gwyneth
      "hans" -> pure Hans
      "ines" -> pure Ines
      "ivy" -> pure Ivy
      "jacek" -> pure Jacek
      "jan" -> pure Jan
      "joanna" -> pure Joanna
      "joey" -> pure Joey
      "justin" -> pure Justin
      "karl" -> pure Karl
      "kendra" -> pure Kendra
      "kevin" -> pure Kevin
      "kimberly" -> pure Kimberly
      "lea" -> pure Lea
      "liv" -> pure Liv
      "lotte" -> pure Lotte
      "lucia" -> pure Lucia
      "lupe" -> pure Lupe
      "mads" -> pure Mads
      "maja" -> pure Maja
      "marlene" -> pure Marlene
      "mathieu" -> pure Mathieu
      "matthew" -> pure Matthew
      "maxim" -> pure Maxim
      "mia" -> pure Mia
      "miguel" -> pure Miguel
      "mizuki" -> pure Mizuki
      "naja" -> pure Naja
      "nicole" -> pure Nicole
      "olivia" -> pure Olivia
      "penelope" -> pure Penelope
      "raveena" -> pure Raveena
      "ricardo" -> pure Ricardo
      "ruben" -> pure Ruben
      "russell" -> pure Russell
      "salli" -> pure Salli
      "seoyeon" -> pure Seoyeon
      "takumi" -> pure Takumi
      "tatyana" -> pure Tatyana
      "vicki" -> pure Vicki
      "vitoria" -> pure Vitoria
      "zeina" -> pure Zeina
      "zhiyu" -> pure Zhiyu
      e ->
        fromTextError $
          "Failure parsing VoiceId from value: '" <> e
            <> "'. Accepted values: aditi, amy, astrid, bianca, brian, camila, carla, carmen, celine, chantal, conchita, cristiano, dora, emma, enrique, ewa, filiz, geraint, giorgio, gwyneth, hans, ines, ivy, jacek, jan, joanna, joey, justin, karl, kendra, kevin, kimberly, lea, liv, lotte, lucia, lupe, mads, maja, marlene, mathieu, matthew, maxim, mia, miguel, mizuki, naja, nicole, olivia, penelope, raveena, ricardo, ruben, russell, salli, seoyeon, takumi, tatyana, vicki, vitoria, zeina, zhiyu"

instance ToText VoiceId where
  toText = \case
    Aditi -> "Aditi"
    Amy -> "Amy"
    Astrid -> "Astrid"
    Bianca -> "Bianca"
    Brian -> "Brian"
    Camila -> "Camila"
    Carla -> "Carla"
    Carmen -> "Carmen"
    Celine -> "Celine"
    Chantal -> "Chantal"
    Conchita -> "Conchita"
    Cristiano -> "Cristiano"
    Dora -> "Dora"
    Emma -> "Emma"
    Enrique -> "Enrique"
    Ewa -> "Ewa"
    Filiz -> "Filiz"
    Geraint -> "Geraint"
    Giorgio -> "Giorgio"
    Gwyneth -> "Gwyneth"
    Hans -> "Hans"
    Ines -> "Ines"
    Ivy -> "Ivy"
    Jacek -> "Jacek"
    Jan -> "Jan"
    Joanna -> "Joanna"
    Joey -> "Joey"
    Justin -> "Justin"
    Karl -> "Karl"
    Kendra -> "Kendra"
    Kevin -> "Kevin"
    Kimberly -> "Kimberly"
    Lea -> "Lea"
    Liv -> "Liv"
    Lotte -> "Lotte"
    Lucia -> "Lucia"
    Lupe -> "Lupe"
    Mads -> "Mads"
    Maja -> "Maja"
    Marlene -> "Marlene"
    Mathieu -> "Mathieu"
    Matthew -> "Matthew"
    Maxim -> "Maxim"
    Mia -> "Mia"
    Miguel -> "Miguel"
    Mizuki -> "Mizuki"
    Naja -> "Naja"
    Nicole -> "Nicole"
    Olivia -> "Olivia"
    Penelope -> "Penelope"
    Raveena -> "Raveena"
    Ricardo -> "Ricardo"
    Ruben -> "Ruben"
    Russell -> "Russell"
    Salli -> "Salli"
    Seoyeon -> "Seoyeon"
    Takumi -> "Takumi"
    Tatyana -> "Tatyana"
    Vicki -> "Vicki"
    Vitoria -> "Vitoria"
    Zeina -> "Zeina"
    Zhiyu -> "Zhiyu"

instance Hashable VoiceId

instance NFData VoiceId

instance ToByteString VoiceId

instance ToQuery VoiceId

instance ToHeader VoiceId

instance ToJSON VoiceId where
  toJSON = toJSONText

instance FromJSON VoiceId where
  parseJSON = parseJSONText "VoiceId"
