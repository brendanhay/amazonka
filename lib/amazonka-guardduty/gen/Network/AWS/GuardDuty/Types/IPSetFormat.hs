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
        ISFTxt,
        ISFStix,
        ISFOtxCSV,
        ISFAlienVault,
        ISFProofPoint,
        ISFFireEye
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

pattern ISFTxt :: IPSetFormat
pattern ISFTxt = IPSetFormat' "TXT"

pattern ISFStix :: IPSetFormat
pattern ISFStix = IPSetFormat' "STIX"

pattern ISFOtxCSV :: IPSetFormat
pattern ISFOtxCSV = IPSetFormat' "OTX_CSV"

pattern ISFAlienVault :: IPSetFormat
pattern ISFAlienVault = IPSetFormat' "ALIEN_VAULT"

pattern ISFProofPoint :: IPSetFormat
pattern ISFProofPoint = IPSetFormat' "PROOF_POINT"

pattern ISFFireEye :: IPSetFormat
pattern ISFFireEye = IPSetFormat' "FIRE_EYE"

{-# COMPLETE
  ISFTxt,
  ISFStix,
  ISFOtxCSV,
  ISFAlienVault,
  ISFProofPoint,
  ISFFireEye,
  IPSetFormat'
  #-}
