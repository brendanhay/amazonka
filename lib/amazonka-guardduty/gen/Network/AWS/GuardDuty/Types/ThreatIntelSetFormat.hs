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
        TISFAlienVault,
        TISFFireEye,
        TISFOtxCSV,
        TISFProofPoint,
        TISFStix,
        TISFTxt
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ThreatIntelSetFormat = ThreatIntelSetFormat' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern TISFAlienVault :: ThreatIntelSetFormat
pattern TISFAlienVault = ThreatIntelSetFormat' "ALIEN_VAULT"

pattern TISFFireEye :: ThreatIntelSetFormat
pattern TISFFireEye = ThreatIntelSetFormat' "FIRE_EYE"

pattern TISFOtxCSV :: ThreatIntelSetFormat
pattern TISFOtxCSV = ThreatIntelSetFormat' "OTX_CSV"

pattern TISFProofPoint :: ThreatIntelSetFormat
pattern TISFProofPoint = ThreatIntelSetFormat' "PROOF_POINT"

pattern TISFStix :: ThreatIntelSetFormat
pattern TISFStix = ThreatIntelSetFormat' "STIX"

pattern TISFTxt :: ThreatIntelSetFormat
pattern TISFTxt = ThreatIntelSetFormat' "TXT"

{-# COMPLETE
  TISFAlienVault,
  TISFFireEye,
  TISFOtxCSV,
  TISFProofPoint,
  TISFStix,
  TISFTxt,
  ThreatIntelSetFormat'
  #-}
