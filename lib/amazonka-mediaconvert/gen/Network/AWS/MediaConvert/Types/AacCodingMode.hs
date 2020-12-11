-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacCodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacCodingMode
  ( AacCodingMode
      ( AacCodingMode',
        ACMAdReceiverMix,
        ACMCodingMode10,
        ACMCodingMode11,
        ACMCodingMode20,
        ACMCodingMode51
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
newtype AacCodingMode = AacCodingMode' Lude.Text
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

pattern ACMAdReceiverMix :: AacCodingMode
pattern ACMAdReceiverMix = AacCodingMode' "AD_RECEIVER_MIX"

pattern ACMCodingMode10 :: AacCodingMode
pattern ACMCodingMode10 = AacCodingMode' "CODING_MODE_1_0"

pattern ACMCodingMode11 :: AacCodingMode
pattern ACMCodingMode11 = AacCodingMode' "CODING_MODE_1_1"

pattern ACMCodingMode20 :: AacCodingMode
pattern ACMCodingMode20 = AacCodingMode' "CODING_MODE_2_0"

pattern ACMCodingMode51 :: AacCodingMode
pattern ACMCodingMode51 = AacCodingMode' "CODING_MODE_5_1"

{-# COMPLETE
  ACMAdReceiverMix,
  ACMCodingMode10,
  ACMCodingMode11,
  ACMCodingMode20,
  ACMCodingMode51,
  AacCodingMode'
  #-}
