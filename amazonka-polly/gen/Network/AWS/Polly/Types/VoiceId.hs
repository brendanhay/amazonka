{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.VoiceId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.VoiceId
  ( VoiceId
      ( ..,
        VoiceId_Aditi,
        VoiceId_Amy,
        VoiceId_Astrid,
        VoiceId_Bianca,
        VoiceId_Brian,
        VoiceId_Camila,
        VoiceId_Carla,
        VoiceId_Carmen,
        VoiceId_Celine,
        VoiceId_Chantal,
        VoiceId_Conchita,
        VoiceId_Cristiano,
        VoiceId_Dora,
        VoiceId_Emma,
        VoiceId_Enrique,
        VoiceId_Ewa,
        VoiceId_Filiz,
        VoiceId_Geraint,
        VoiceId_Giorgio,
        VoiceId_Gwyneth,
        VoiceId_Hans,
        VoiceId_Ines,
        VoiceId_Ivy,
        VoiceId_Jacek,
        VoiceId_Jan,
        VoiceId_Joanna,
        VoiceId_Joey,
        VoiceId_Justin,
        VoiceId_Karl,
        VoiceId_Kendra,
        VoiceId_Kevin,
        VoiceId_Kimberly,
        VoiceId_Lea,
        VoiceId_Liv,
        VoiceId_Lotte,
        VoiceId_Lucia,
        VoiceId_Lupe,
        VoiceId_Mads,
        VoiceId_Maja,
        VoiceId_Marlene,
        VoiceId_Mathieu,
        VoiceId_Matthew,
        VoiceId_Maxim,
        VoiceId_Mia,
        VoiceId_Miguel,
        VoiceId_Mizuki,
        VoiceId_Naja,
        VoiceId_Nicole,
        VoiceId_Olivia,
        VoiceId_Penelope,
        VoiceId_Raveena,
        VoiceId_Ricardo,
        VoiceId_Ruben,
        VoiceId_Russell,
        VoiceId_Salli,
        VoiceId_Seoyeon,
        VoiceId_Takumi,
        VoiceId_Tatyana,
        VoiceId_Vicki,
        VoiceId_Vitoria,
        VoiceId_Zeina,
        VoiceId_Zhiyu
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype VoiceId = VoiceId' {fromVoiceId :: Core.Text}
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern VoiceId_Aditi :: VoiceId
pattern VoiceId_Aditi = VoiceId' "Aditi"

pattern VoiceId_Amy :: VoiceId
pattern VoiceId_Amy = VoiceId' "Amy"

pattern VoiceId_Astrid :: VoiceId
pattern VoiceId_Astrid = VoiceId' "Astrid"

pattern VoiceId_Bianca :: VoiceId
pattern VoiceId_Bianca = VoiceId' "Bianca"

pattern VoiceId_Brian :: VoiceId
pattern VoiceId_Brian = VoiceId' "Brian"

pattern VoiceId_Camila :: VoiceId
pattern VoiceId_Camila = VoiceId' "Camila"

pattern VoiceId_Carla :: VoiceId
pattern VoiceId_Carla = VoiceId' "Carla"

pattern VoiceId_Carmen :: VoiceId
pattern VoiceId_Carmen = VoiceId' "Carmen"

pattern VoiceId_Celine :: VoiceId
pattern VoiceId_Celine = VoiceId' "Celine"

pattern VoiceId_Chantal :: VoiceId
pattern VoiceId_Chantal = VoiceId' "Chantal"

pattern VoiceId_Conchita :: VoiceId
pattern VoiceId_Conchita = VoiceId' "Conchita"

pattern VoiceId_Cristiano :: VoiceId
pattern VoiceId_Cristiano = VoiceId' "Cristiano"

pattern VoiceId_Dora :: VoiceId
pattern VoiceId_Dora = VoiceId' "Dora"

pattern VoiceId_Emma :: VoiceId
pattern VoiceId_Emma = VoiceId' "Emma"

pattern VoiceId_Enrique :: VoiceId
pattern VoiceId_Enrique = VoiceId' "Enrique"

pattern VoiceId_Ewa :: VoiceId
pattern VoiceId_Ewa = VoiceId' "Ewa"

pattern VoiceId_Filiz :: VoiceId
pattern VoiceId_Filiz = VoiceId' "Filiz"

pattern VoiceId_Geraint :: VoiceId
pattern VoiceId_Geraint = VoiceId' "Geraint"

pattern VoiceId_Giorgio :: VoiceId
pattern VoiceId_Giorgio = VoiceId' "Giorgio"

pattern VoiceId_Gwyneth :: VoiceId
pattern VoiceId_Gwyneth = VoiceId' "Gwyneth"

pattern VoiceId_Hans :: VoiceId
pattern VoiceId_Hans = VoiceId' "Hans"

pattern VoiceId_Ines :: VoiceId
pattern VoiceId_Ines = VoiceId' "Ines"

pattern VoiceId_Ivy :: VoiceId
pattern VoiceId_Ivy = VoiceId' "Ivy"

pattern VoiceId_Jacek :: VoiceId
pattern VoiceId_Jacek = VoiceId' "Jacek"

pattern VoiceId_Jan :: VoiceId
pattern VoiceId_Jan = VoiceId' "Jan"

pattern VoiceId_Joanna :: VoiceId
pattern VoiceId_Joanna = VoiceId' "Joanna"

pattern VoiceId_Joey :: VoiceId
pattern VoiceId_Joey = VoiceId' "Joey"

pattern VoiceId_Justin :: VoiceId
pattern VoiceId_Justin = VoiceId' "Justin"

pattern VoiceId_Karl :: VoiceId
pattern VoiceId_Karl = VoiceId' "Karl"

pattern VoiceId_Kendra :: VoiceId
pattern VoiceId_Kendra = VoiceId' "Kendra"

pattern VoiceId_Kevin :: VoiceId
pattern VoiceId_Kevin = VoiceId' "Kevin"

pattern VoiceId_Kimberly :: VoiceId
pattern VoiceId_Kimberly = VoiceId' "Kimberly"

pattern VoiceId_Lea :: VoiceId
pattern VoiceId_Lea = VoiceId' "Lea"

pattern VoiceId_Liv :: VoiceId
pattern VoiceId_Liv = VoiceId' "Liv"

pattern VoiceId_Lotte :: VoiceId
pattern VoiceId_Lotte = VoiceId' "Lotte"

pattern VoiceId_Lucia :: VoiceId
pattern VoiceId_Lucia = VoiceId' "Lucia"

pattern VoiceId_Lupe :: VoiceId
pattern VoiceId_Lupe = VoiceId' "Lupe"

pattern VoiceId_Mads :: VoiceId
pattern VoiceId_Mads = VoiceId' "Mads"

pattern VoiceId_Maja :: VoiceId
pattern VoiceId_Maja = VoiceId' "Maja"

pattern VoiceId_Marlene :: VoiceId
pattern VoiceId_Marlene = VoiceId' "Marlene"

pattern VoiceId_Mathieu :: VoiceId
pattern VoiceId_Mathieu = VoiceId' "Mathieu"

pattern VoiceId_Matthew :: VoiceId
pattern VoiceId_Matthew = VoiceId' "Matthew"

pattern VoiceId_Maxim :: VoiceId
pattern VoiceId_Maxim = VoiceId' "Maxim"

pattern VoiceId_Mia :: VoiceId
pattern VoiceId_Mia = VoiceId' "Mia"

pattern VoiceId_Miguel :: VoiceId
pattern VoiceId_Miguel = VoiceId' "Miguel"

pattern VoiceId_Mizuki :: VoiceId
pattern VoiceId_Mizuki = VoiceId' "Mizuki"

pattern VoiceId_Naja :: VoiceId
pattern VoiceId_Naja = VoiceId' "Naja"

pattern VoiceId_Nicole :: VoiceId
pattern VoiceId_Nicole = VoiceId' "Nicole"

pattern VoiceId_Olivia :: VoiceId
pattern VoiceId_Olivia = VoiceId' "Olivia"

pattern VoiceId_Penelope :: VoiceId
pattern VoiceId_Penelope = VoiceId' "Penelope"

pattern VoiceId_Raveena :: VoiceId
pattern VoiceId_Raveena = VoiceId' "Raveena"

pattern VoiceId_Ricardo :: VoiceId
pattern VoiceId_Ricardo = VoiceId' "Ricardo"

pattern VoiceId_Ruben :: VoiceId
pattern VoiceId_Ruben = VoiceId' "Ruben"

pattern VoiceId_Russell :: VoiceId
pattern VoiceId_Russell = VoiceId' "Russell"

pattern VoiceId_Salli :: VoiceId
pattern VoiceId_Salli = VoiceId' "Salli"

pattern VoiceId_Seoyeon :: VoiceId
pattern VoiceId_Seoyeon = VoiceId' "Seoyeon"

pattern VoiceId_Takumi :: VoiceId
pattern VoiceId_Takumi = VoiceId' "Takumi"

pattern VoiceId_Tatyana :: VoiceId
pattern VoiceId_Tatyana = VoiceId' "Tatyana"

pattern VoiceId_Vicki :: VoiceId
pattern VoiceId_Vicki = VoiceId' "Vicki"

pattern VoiceId_Vitoria :: VoiceId
pattern VoiceId_Vitoria = VoiceId' "Vitoria"

pattern VoiceId_Zeina :: VoiceId
pattern VoiceId_Zeina = VoiceId' "Zeina"

pattern VoiceId_Zhiyu :: VoiceId
pattern VoiceId_Zhiyu = VoiceId' "Zhiyu"

{-# COMPLETE
  VoiceId_Aditi,
  VoiceId_Amy,
  VoiceId_Astrid,
  VoiceId_Bianca,
  VoiceId_Brian,
  VoiceId_Camila,
  VoiceId_Carla,
  VoiceId_Carmen,
  VoiceId_Celine,
  VoiceId_Chantal,
  VoiceId_Conchita,
  VoiceId_Cristiano,
  VoiceId_Dora,
  VoiceId_Emma,
  VoiceId_Enrique,
  VoiceId_Ewa,
  VoiceId_Filiz,
  VoiceId_Geraint,
  VoiceId_Giorgio,
  VoiceId_Gwyneth,
  VoiceId_Hans,
  VoiceId_Ines,
  VoiceId_Ivy,
  VoiceId_Jacek,
  VoiceId_Jan,
  VoiceId_Joanna,
  VoiceId_Joey,
  VoiceId_Justin,
  VoiceId_Karl,
  VoiceId_Kendra,
  VoiceId_Kevin,
  VoiceId_Kimberly,
  VoiceId_Lea,
  VoiceId_Liv,
  VoiceId_Lotte,
  VoiceId_Lucia,
  VoiceId_Lupe,
  VoiceId_Mads,
  VoiceId_Maja,
  VoiceId_Marlene,
  VoiceId_Mathieu,
  VoiceId_Matthew,
  VoiceId_Maxim,
  VoiceId_Mia,
  VoiceId_Miguel,
  VoiceId_Mizuki,
  VoiceId_Naja,
  VoiceId_Nicole,
  VoiceId_Olivia,
  VoiceId_Penelope,
  VoiceId_Raveena,
  VoiceId_Ricardo,
  VoiceId_Ruben,
  VoiceId_Russell,
  VoiceId_Salli,
  VoiceId_Seoyeon,
  VoiceId_Takumi,
  VoiceId_Tatyana,
  VoiceId_Vicki,
  VoiceId_Vitoria,
  VoiceId_Zeina,
  VoiceId_Zhiyu,
  VoiceId'
  #-}
