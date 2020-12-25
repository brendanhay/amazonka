{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelSetFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelSetFormat
  ( ThreatIntelSetFormat
      ( ThreatIntelSetFormat',
        ThreatIntelSetFormatTxt,
        ThreatIntelSetFormatStix,
        ThreatIntelSetFormatOtxCsv,
        ThreatIntelSetFormatAlienVault,
        ThreatIntelSetFormatProofPoint,
        ThreatIntelSetFormatFireEye,
        fromThreatIntelSetFormat
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ThreatIntelSetFormat = ThreatIntelSetFormat'
  { fromThreatIntelSetFormat ::
      Core.Text
  }
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

pattern ThreatIntelSetFormatTxt :: ThreatIntelSetFormat
pattern ThreatIntelSetFormatTxt = ThreatIntelSetFormat' "TXT"

pattern ThreatIntelSetFormatStix :: ThreatIntelSetFormat
pattern ThreatIntelSetFormatStix = ThreatIntelSetFormat' "STIX"

pattern ThreatIntelSetFormatOtxCsv :: ThreatIntelSetFormat
pattern ThreatIntelSetFormatOtxCsv = ThreatIntelSetFormat' "OTX_CSV"

pattern ThreatIntelSetFormatAlienVault :: ThreatIntelSetFormat
pattern ThreatIntelSetFormatAlienVault = ThreatIntelSetFormat' "ALIEN_VAULT"

pattern ThreatIntelSetFormatProofPoint :: ThreatIntelSetFormat
pattern ThreatIntelSetFormatProofPoint = ThreatIntelSetFormat' "PROOF_POINT"

pattern ThreatIntelSetFormatFireEye :: ThreatIntelSetFormat
pattern ThreatIntelSetFormatFireEye = ThreatIntelSetFormat' "FIRE_EYE"

{-# COMPLETE
  ThreatIntelSetFormatTxt,
  ThreatIntelSetFormatStix,
  ThreatIntelSetFormatOtxCsv,
  ThreatIntelSetFormatAlienVault,
  ThreatIntelSetFormatProofPoint,
  ThreatIntelSetFormatFireEye,
  ThreatIntelSetFormat'
  #-}
