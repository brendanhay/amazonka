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
        Txt,
        Stix,
        OtxCSV,
        AlienVault,
        ProofPoint,
        FireEye
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

pattern Txt :: ThreatIntelSetFormat
pattern Txt = ThreatIntelSetFormat' "TXT"

pattern Stix :: ThreatIntelSetFormat
pattern Stix = ThreatIntelSetFormat' "STIX"

pattern OtxCSV :: ThreatIntelSetFormat
pattern OtxCSV = ThreatIntelSetFormat' "OTX_CSV"

pattern AlienVault :: ThreatIntelSetFormat
pattern AlienVault = ThreatIntelSetFormat' "ALIEN_VAULT"

pattern ProofPoint :: ThreatIntelSetFormat
pattern ProofPoint = ThreatIntelSetFormat' "PROOF_POINT"

pattern FireEye :: ThreatIntelSetFormat
pattern FireEye = ThreatIntelSetFormat' "FIRE_EYE"

{-# COMPLETE
  Txt,
  Stix,
  OtxCSV,
  AlienVault,
  ProofPoint,
  FireEye,
  ThreatIntelSetFormat'
  #-}
