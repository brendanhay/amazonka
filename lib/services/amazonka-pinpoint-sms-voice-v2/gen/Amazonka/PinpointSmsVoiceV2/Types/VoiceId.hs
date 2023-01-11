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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.VoiceId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.VoiceId
  ( VoiceId
      ( ..,
        VoiceId_AMY,
        VoiceId_ASTRID,
        VoiceId_BIANCA,
        VoiceId_BRIAN,
        VoiceId_CAMILA,
        VoiceId_CARLA,
        VoiceId_CARMEN,
        VoiceId_CELINE,
        VoiceId_CHANTAL,
        VoiceId_CONCHITA,
        VoiceId_CRISTIANO,
        VoiceId_DORA,
        VoiceId_EMMA,
        VoiceId_ENRIQUE,
        VoiceId_EWA,
        VoiceId_FILIZ,
        VoiceId_GERAINT,
        VoiceId_GIORGIO,
        VoiceId_GWYNETH,
        VoiceId_HANS,
        VoiceId_INES,
        VoiceId_IVY,
        VoiceId_JACEK,
        VoiceId_JAN,
        VoiceId_JOANNA,
        VoiceId_JOEY,
        VoiceId_JUSTIN,
        VoiceId_KARL,
        VoiceId_KENDRA,
        VoiceId_KIMBERLY,
        VoiceId_LEA,
        VoiceId_LIV,
        VoiceId_LOTTE,
        VoiceId_LUCIA,
        VoiceId_LUPE,
        VoiceId_MADS,
        VoiceId_MAJA,
        VoiceId_MARLENE,
        VoiceId_MATHIEU,
        VoiceId_MATTHEW,
        VoiceId_MAXIM,
        VoiceId_MIA,
        VoiceId_MIGUEL,
        VoiceId_MIZUKI,
        VoiceId_NAJA,
        VoiceId_NICOLE,
        VoiceId_PENELOPE,
        VoiceId_RAVEENA,
        VoiceId_RICARDO,
        VoiceId_RUBEN,
        VoiceId_RUSSELL,
        VoiceId_SALLI,
        VoiceId_SEOYEON,
        VoiceId_TAKUMI,
        VoiceId_TATYANA,
        VoiceId_VICKI,
        VoiceId_VITORIA,
        VoiceId_ZEINA,
        VoiceId_ZHIYU
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VoiceId = VoiceId' {fromVoiceId :: Data.Text}
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern VoiceId_AMY :: VoiceId
pattern VoiceId_AMY = VoiceId' "AMY"

pattern VoiceId_ASTRID :: VoiceId
pattern VoiceId_ASTRID = VoiceId' "ASTRID"

pattern VoiceId_BIANCA :: VoiceId
pattern VoiceId_BIANCA = VoiceId' "BIANCA"

pattern VoiceId_BRIAN :: VoiceId
pattern VoiceId_BRIAN = VoiceId' "BRIAN"

pattern VoiceId_CAMILA :: VoiceId
pattern VoiceId_CAMILA = VoiceId' "CAMILA"

pattern VoiceId_CARLA :: VoiceId
pattern VoiceId_CARLA = VoiceId' "CARLA"

pattern VoiceId_CARMEN :: VoiceId
pattern VoiceId_CARMEN = VoiceId' "CARMEN"

pattern VoiceId_CELINE :: VoiceId
pattern VoiceId_CELINE = VoiceId' "CELINE"

pattern VoiceId_CHANTAL :: VoiceId
pattern VoiceId_CHANTAL = VoiceId' "CHANTAL"

pattern VoiceId_CONCHITA :: VoiceId
pattern VoiceId_CONCHITA = VoiceId' "CONCHITA"

pattern VoiceId_CRISTIANO :: VoiceId
pattern VoiceId_CRISTIANO = VoiceId' "CRISTIANO"

pattern VoiceId_DORA :: VoiceId
pattern VoiceId_DORA = VoiceId' "DORA"

pattern VoiceId_EMMA :: VoiceId
pattern VoiceId_EMMA = VoiceId' "EMMA"

pattern VoiceId_ENRIQUE :: VoiceId
pattern VoiceId_ENRIQUE = VoiceId' "ENRIQUE"

pattern VoiceId_EWA :: VoiceId
pattern VoiceId_EWA = VoiceId' "EWA"

pattern VoiceId_FILIZ :: VoiceId
pattern VoiceId_FILIZ = VoiceId' "FILIZ"

pattern VoiceId_GERAINT :: VoiceId
pattern VoiceId_GERAINT = VoiceId' "GERAINT"

pattern VoiceId_GIORGIO :: VoiceId
pattern VoiceId_GIORGIO = VoiceId' "GIORGIO"

pattern VoiceId_GWYNETH :: VoiceId
pattern VoiceId_GWYNETH = VoiceId' "GWYNETH"

pattern VoiceId_HANS :: VoiceId
pattern VoiceId_HANS = VoiceId' "HANS"

pattern VoiceId_INES :: VoiceId
pattern VoiceId_INES = VoiceId' "INES"

pattern VoiceId_IVY :: VoiceId
pattern VoiceId_IVY = VoiceId' "IVY"

pattern VoiceId_JACEK :: VoiceId
pattern VoiceId_JACEK = VoiceId' "JACEK"

pattern VoiceId_JAN :: VoiceId
pattern VoiceId_JAN = VoiceId' "JAN"

pattern VoiceId_JOANNA :: VoiceId
pattern VoiceId_JOANNA = VoiceId' "JOANNA"

pattern VoiceId_JOEY :: VoiceId
pattern VoiceId_JOEY = VoiceId' "JOEY"

pattern VoiceId_JUSTIN :: VoiceId
pattern VoiceId_JUSTIN = VoiceId' "JUSTIN"

pattern VoiceId_KARL :: VoiceId
pattern VoiceId_KARL = VoiceId' "KARL"

pattern VoiceId_KENDRA :: VoiceId
pattern VoiceId_KENDRA = VoiceId' "KENDRA"

pattern VoiceId_KIMBERLY :: VoiceId
pattern VoiceId_KIMBERLY = VoiceId' "KIMBERLY"

pattern VoiceId_LEA :: VoiceId
pattern VoiceId_LEA = VoiceId' "LEA"

pattern VoiceId_LIV :: VoiceId
pattern VoiceId_LIV = VoiceId' "LIV"

pattern VoiceId_LOTTE :: VoiceId
pattern VoiceId_LOTTE = VoiceId' "LOTTE"

pattern VoiceId_LUCIA :: VoiceId
pattern VoiceId_LUCIA = VoiceId' "LUCIA"

pattern VoiceId_LUPE :: VoiceId
pattern VoiceId_LUPE = VoiceId' "LUPE"

pattern VoiceId_MADS :: VoiceId
pattern VoiceId_MADS = VoiceId' "MADS"

pattern VoiceId_MAJA :: VoiceId
pattern VoiceId_MAJA = VoiceId' "MAJA"

pattern VoiceId_MARLENE :: VoiceId
pattern VoiceId_MARLENE = VoiceId' "MARLENE"

pattern VoiceId_MATHIEU :: VoiceId
pattern VoiceId_MATHIEU = VoiceId' "MATHIEU"

pattern VoiceId_MATTHEW :: VoiceId
pattern VoiceId_MATTHEW = VoiceId' "MATTHEW"

pattern VoiceId_MAXIM :: VoiceId
pattern VoiceId_MAXIM = VoiceId' "MAXIM"

pattern VoiceId_MIA :: VoiceId
pattern VoiceId_MIA = VoiceId' "MIA"

pattern VoiceId_MIGUEL :: VoiceId
pattern VoiceId_MIGUEL = VoiceId' "MIGUEL"

pattern VoiceId_MIZUKI :: VoiceId
pattern VoiceId_MIZUKI = VoiceId' "MIZUKI"

pattern VoiceId_NAJA :: VoiceId
pattern VoiceId_NAJA = VoiceId' "NAJA"

pattern VoiceId_NICOLE :: VoiceId
pattern VoiceId_NICOLE = VoiceId' "NICOLE"

pattern VoiceId_PENELOPE :: VoiceId
pattern VoiceId_PENELOPE = VoiceId' "PENELOPE"

pattern VoiceId_RAVEENA :: VoiceId
pattern VoiceId_RAVEENA = VoiceId' "RAVEENA"

pattern VoiceId_RICARDO :: VoiceId
pattern VoiceId_RICARDO = VoiceId' "RICARDO"

pattern VoiceId_RUBEN :: VoiceId
pattern VoiceId_RUBEN = VoiceId' "RUBEN"

pattern VoiceId_RUSSELL :: VoiceId
pattern VoiceId_RUSSELL = VoiceId' "RUSSELL"

pattern VoiceId_SALLI :: VoiceId
pattern VoiceId_SALLI = VoiceId' "SALLI"

pattern VoiceId_SEOYEON :: VoiceId
pattern VoiceId_SEOYEON = VoiceId' "SEOYEON"

pattern VoiceId_TAKUMI :: VoiceId
pattern VoiceId_TAKUMI = VoiceId' "TAKUMI"

pattern VoiceId_TATYANA :: VoiceId
pattern VoiceId_TATYANA = VoiceId' "TATYANA"

pattern VoiceId_VICKI :: VoiceId
pattern VoiceId_VICKI = VoiceId' "VICKI"

pattern VoiceId_VITORIA :: VoiceId
pattern VoiceId_VITORIA = VoiceId' "VITORIA"

pattern VoiceId_ZEINA :: VoiceId
pattern VoiceId_ZEINA = VoiceId' "ZEINA"

pattern VoiceId_ZHIYU :: VoiceId
pattern VoiceId_ZHIYU = VoiceId' "ZHIYU"

{-# COMPLETE
  VoiceId_AMY,
  VoiceId_ASTRID,
  VoiceId_BIANCA,
  VoiceId_BRIAN,
  VoiceId_CAMILA,
  VoiceId_CARLA,
  VoiceId_CARMEN,
  VoiceId_CELINE,
  VoiceId_CHANTAL,
  VoiceId_CONCHITA,
  VoiceId_CRISTIANO,
  VoiceId_DORA,
  VoiceId_EMMA,
  VoiceId_ENRIQUE,
  VoiceId_EWA,
  VoiceId_FILIZ,
  VoiceId_GERAINT,
  VoiceId_GIORGIO,
  VoiceId_GWYNETH,
  VoiceId_HANS,
  VoiceId_INES,
  VoiceId_IVY,
  VoiceId_JACEK,
  VoiceId_JAN,
  VoiceId_JOANNA,
  VoiceId_JOEY,
  VoiceId_JUSTIN,
  VoiceId_KARL,
  VoiceId_KENDRA,
  VoiceId_KIMBERLY,
  VoiceId_LEA,
  VoiceId_LIV,
  VoiceId_LOTTE,
  VoiceId_LUCIA,
  VoiceId_LUPE,
  VoiceId_MADS,
  VoiceId_MAJA,
  VoiceId_MARLENE,
  VoiceId_MATHIEU,
  VoiceId_MATTHEW,
  VoiceId_MAXIM,
  VoiceId_MIA,
  VoiceId_MIGUEL,
  VoiceId_MIZUKI,
  VoiceId_NAJA,
  VoiceId_NICOLE,
  VoiceId_PENELOPE,
  VoiceId_RAVEENA,
  VoiceId_RICARDO,
  VoiceId_RUBEN,
  VoiceId_RUSSELL,
  VoiceId_SALLI,
  VoiceId_SEOYEON,
  VoiceId_TAKUMI,
  VoiceId_TATYANA,
  VoiceId_VICKI,
  VoiceId_VITORIA,
  VoiceId_ZEINA,
  VoiceId_ZHIYU,
  VoiceId'
  #-}
