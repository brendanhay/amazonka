{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IPSetFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.IPSetFormat
  ( IPSetFormat
      ( IPSetFormat',
        AlienVault,
        FireEye,
        OtxCSV,
        ProofPoint,
        Stix,
        Txt
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype IPSetFormat = IPSetFormat' Lude.Text
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

pattern AlienVault :: IPSetFormat
pattern AlienVault = IPSetFormat' "ALIEN_VAULT"

pattern FireEye :: IPSetFormat
pattern FireEye = IPSetFormat' "FIRE_EYE"

pattern OtxCSV :: IPSetFormat
pattern OtxCSV = IPSetFormat' "OTX_CSV"

pattern ProofPoint :: IPSetFormat
pattern ProofPoint = IPSetFormat' "PROOF_POINT"

pattern Stix :: IPSetFormat
pattern Stix = IPSetFormat' "STIX"

pattern Txt :: IPSetFormat
pattern Txt = IPSetFormat' "TXT"

{-# COMPLETE
  AlienVault,
  FireEye,
  OtxCSV,
  ProofPoint,
  Stix,
  Txt,
  IPSetFormat'
  #-}
