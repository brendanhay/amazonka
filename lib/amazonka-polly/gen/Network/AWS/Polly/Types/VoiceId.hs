{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.VoiceId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.VoiceId
  ( VoiceId
      ( VoiceId',
        VoiceIdAditi,
        VoiceIdAmy,
        VoiceIdAstrid,
        VoiceIdBianca,
        VoiceIdBrian,
        VoiceIdCamila,
        VoiceIdCarla,
        VoiceIdCarmen,
        VoiceIdCeline,
        VoiceIdChantal,
        VoiceIdConchita,
        VoiceIdCristiano,
        VoiceIdDora,
        VoiceIdEmma,
        VoiceIdEnrique,
        VoiceIdEwa,
        VoiceIdFiliz,
        VoiceIdGeraint,
        VoiceIdGiorgio,
        VoiceIdGwyneth,
        VoiceIdHans,
        VoiceIdInes,
        VoiceIdIvy,
        VoiceIdJacek,
        VoiceIdJan,
        VoiceIdJoanna,
        VoiceIdJoey,
        VoiceIdJustin,
        VoiceIdKarl,
        VoiceIdKendra,
        VoiceIdKevin,
        VoiceIdKimberly,
        VoiceIdLea,
        VoiceIdLiv,
        VoiceIdLotte,
        VoiceIdLucia,
        VoiceIdLupe,
        VoiceIdMads,
        VoiceIdMaja,
        VoiceIdMarlene,
        VoiceIdMathieu,
        VoiceIdMatthew,
        VoiceIdMaxim,
        VoiceIdMia,
        VoiceIdMiguel,
        VoiceIdMizuki,
        VoiceIdNaja,
        VoiceIdNicole,
        VoiceIdOlivia,
        VoiceIdPenelope,
        VoiceIdRaveena,
        VoiceIdRicardo,
        VoiceIdRuben,
        VoiceIdRussell,
        VoiceIdSalli,
        VoiceIdSeoyeon,
        VoiceIdTakumi,
        VoiceIdTatyana,
        VoiceIdVicki,
        VoiceIdVitoria,
        VoiceIdZeina,
        VoiceIdZhiyu,
        fromVoiceId
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype VoiceId = VoiceId' {fromVoiceId :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern VoiceIdAditi :: VoiceId
pattern VoiceIdAditi = VoiceId' "Aditi"

pattern VoiceIdAmy :: VoiceId
pattern VoiceIdAmy = VoiceId' "Amy"

pattern VoiceIdAstrid :: VoiceId
pattern VoiceIdAstrid = VoiceId' "Astrid"

pattern VoiceIdBianca :: VoiceId
pattern VoiceIdBianca = VoiceId' "Bianca"

pattern VoiceIdBrian :: VoiceId
pattern VoiceIdBrian = VoiceId' "Brian"

pattern VoiceIdCamila :: VoiceId
pattern VoiceIdCamila = VoiceId' "Camila"

pattern VoiceIdCarla :: VoiceId
pattern VoiceIdCarla = VoiceId' "Carla"

pattern VoiceIdCarmen :: VoiceId
pattern VoiceIdCarmen = VoiceId' "Carmen"

pattern VoiceIdCeline :: VoiceId
pattern VoiceIdCeline = VoiceId' "Celine"

pattern VoiceIdChantal :: VoiceId
pattern VoiceIdChantal = VoiceId' "Chantal"

pattern VoiceIdConchita :: VoiceId
pattern VoiceIdConchita = VoiceId' "Conchita"

pattern VoiceIdCristiano :: VoiceId
pattern VoiceIdCristiano = VoiceId' "Cristiano"

pattern VoiceIdDora :: VoiceId
pattern VoiceIdDora = VoiceId' "Dora"

pattern VoiceIdEmma :: VoiceId
pattern VoiceIdEmma = VoiceId' "Emma"

pattern VoiceIdEnrique :: VoiceId
pattern VoiceIdEnrique = VoiceId' "Enrique"

pattern VoiceIdEwa :: VoiceId
pattern VoiceIdEwa = VoiceId' "Ewa"

pattern VoiceIdFiliz :: VoiceId
pattern VoiceIdFiliz = VoiceId' "Filiz"

pattern VoiceIdGeraint :: VoiceId
pattern VoiceIdGeraint = VoiceId' "Geraint"

pattern VoiceIdGiorgio :: VoiceId
pattern VoiceIdGiorgio = VoiceId' "Giorgio"

pattern VoiceIdGwyneth :: VoiceId
pattern VoiceIdGwyneth = VoiceId' "Gwyneth"

pattern VoiceIdHans :: VoiceId
pattern VoiceIdHans = VoiceId' "Hans"

pattern VoiceIdInes :: VoiceId
pattern VoiceIdInes = VoiceId' "Ines"

pattern VoiceIdIvy :: VoiceId
pattern VoiceIdIvy = VoiceId' "Ivy"

pattern VoiceIdJacek :: VoiceId
pattern VoiceIdJacek = VoiceId' "Jacek"

pattern VoiceIdJan :: VoiceId
pattern VoiceIdJan = VoiceId' "Jan"

pattern VoiceIdJoanna :: VoiceId
pattern VoiceIdJoanna = VoiceId' "Joanna"

pattern VoiceIdJoey :: VoiceId
pattern VoiceIdJoey = VoiceId' "Joey"

pattern VoiceIdJustin :: VoiceId
pattern VoiceIdJustin = VoiceId' "Justin"

pattern VoiceIdKarl :: VoiceId
pattern VoiceIdKarl = VoiceId' "Karl"

pattern VoiceIdKendra :: VoiceId
pattern VoiceIdKendra = VoiceId' "Kendra"

pattern VoiceIdKevin :: VoiceId
pattern VoiceIdKevin = VoiceId' "Kevin"

pattern VoiceIdKimberly :: VoiceId
pattern VoiceIdKimberly = VoiceId' "Kimberly"

pattern VoiceIdLea :: VoiceId
pattern VoiceIdLea = VoiceId' "Lea"

pattern VoiceIdLiv :: VoiceId
pattern VoiceIdLiv = VoiceId' "Liv"

pattern VoiceIdLotte :: VoiceId
pattern VoiceIdLotte = VoiceId' "Lotte"

pattern VoiceIdLucia :: VoiceId
pattern VoiceIdLucia = VoiceId' "Lucia"

pattern VoiceIdLupe :: VoiceId
pattern VoiceIdLupe = VoiceId' "Lupe"

pattern VoiceIdMads :: VoiceId
pattern VoiceIdMads = VoiceId' "Mads"

pattern VoiceIdMaja :: VoiceId
pattern VoiceIdMaja = VoiceId' "Maja"

pattern VoiceIdMarlene :: VoiceId
pattern VoiceIdMarlene = VoiceId' "Marlene"

pattern VoiceIdMathieu :: VoiceId
pattern VoiceIdMathieu = VoiceId' "Mathieu"

pattern VoiceIdMatthew :: VoiceId
pattern VoiceIdMatthew = VoiceId' "Matthew"

pattern VoiceIdMaxim :: VoiceId
pattern VoiceIdMaxim = VoiceId' "Maxim"

pattern VoiceIdMia :: VoiceId
pattern VoiceIdMia = VoiceId' "Mia"

pattern VoiceIdMiguel :: VoiceId
pattern VoiceIdMiguel = VoiceId' "Miguel"

pattern VoiceIdMizuki :: VoiceId
pattern VoiceIdMizuki = VoiceId' "Mizuki"

pattern VoiceIdNaja :: VoiceId
pattern VoiceIdNaja = VoiceId' "Naja"

pattern VoiceIdNicole :: VoiceId
pattern VoiceIdNicole = VoiceId' "Nicole"

pattern VoiceIdOlivia :: VoiceId
pattern VoiceIdOlivia = VoiceId' "Olivia"

pattern VoiceIdPenelope :: VoiceId
pattern VoiceIdPenelope = VoiceId' "Penelope"

pattern VoiceIdRaveena :: VoiceId
pattern VoiceIdRaveena = VoiceId' "Raveena"

pattern VoiceIdRicardo :: VoiceId
pattern VoiceIdRicardo = VoiceId' "Ricardo"

pattern VoiceIdRuben :: VoiceId
pattern VoiceIdRuben = VoiceId' "Ruben"

pattern VoiceIdRussell :: VoiceId
pattern VoiceIdRussell = VoiceId' "Russell"

pattern VoiceIdSalli :: VoiceId
pattern VoiceIdSalli = VoiceId' "Salli"

pattern VoiceIdSeoyeon :: VoiceId
pattern VoiceIdSeoyeon = VoiceId' "Seoyeon"

pattern VoiceIdTakumi :: VoiceId
pattern VoiceIdTakumi = VoiceId' "Takumi"

pattern VoiceIdTatyana :: VoiceId
pattern VoiceIdTatyana = VoiceId' "Tatyana"

pattern VoiceIdVicki :: VoiceId
pattern VoiceIdVicki = VoiceId' "Vicki"

pattern VoiceIdVitoria :: VoiceId
pattern VoiceIdVitoria = VoiceId' "Vitoria"

pattern VoiceIdZeina :: VoiceId
pattern VoiceIdZeina = VoiceId' "Zeina"

pattern VoiceIdZhiyu :: VoiceId
pattern VoiceIdZhiyu = VoiceId' "Zhiyu"

{-# COMPLETE
  VoiceIdAditi,
  VoiceIdAmy,
  VoiceIdAstrid,
  VoiceIdBianca,
  VoiceIdBrian,
  VoiceIdCamila,
  VoiceIdCarla,
  VoiceIdCarmen,
  VoiceIdCeline,
  VoiceIdChantal,
  VoiceIdConchita,
  VoiceIdCristiano,
  VoiceIdDora,
  VoiceIdEmma,
  VoiceIdEnrique,
  VoiceIdEwa,
  VoiceIdFiliz,
  VoiceIdGeraint,
  VoiceIdGiorgio,
  VoiceIdGwyneth,
  VoiceIdHans,
  VoiceIdInes,
  VoiceIdIvy,
  VoiceIdJacek,
  VoiceIdJan,
  VoiceIdJoanna,
  VoiceIdJoey,
  VoiceIdJustin,
  VoiceIdKarl,
  VoiceIdKendra,
  VoiceIdKevin,
  VoiceIdKimberly,
  VoiceIdLea,
  VoiceIdLiv,
  VoiceIdLotte,
  VoiceIdLucia,
  VoiceIdLupe,
  VoiceIdMads,
  VoiceIdMaja,
  VoiceIdMarlene,
  VoiceIdMathieu,
  VoiceIdMatthew,
  VoiceIdMaxim,
  VoiceIdMia,
  VoiceIdMiguel,
  VoiceIdMizuki,
  VoiceIdNaja,
  VoiceIdNicole,
  VoiceIdOlivia,
  VoiceIdPenelope,
  VoiceIdRaveena,
  VoiceIdRicardo,
  VoiceIdRuben,
  VoiceIdRussell,
  VoiceIdSalli,
  VoiceIdSeoyeon,
  VoiceIdTakumi,
  VoiceIdTatyana,
  VoiceIdVicki,
  VoiceIdVitoria,
  VoiceIdZeina,
  VoiceIdZhiyu,
  VoiceId'
  #-}
