{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        AacCodingModeAdReceiverMix,
        AacCodingModeCodingMode10,
        AacCodingModeCodingMode11,
        AacCodingModeCodingMode20,
        AacCodingModeCodingMode51,
        fromAacCodingMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
newtype AacCodingMode = AacCodingMode'
  { fromAacCodingMode ::
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

pattern AacCodingModeAdReceiverMix :: AacCodingMode
pattern AacCodingModeAdReceiverMix = AacCodingMode' "AD_RECEIVER_MIX"

pattern AacCodingModeCodingMode10 :: AacCodingMode
pattern AacCodingModeCodingMode10 = AacCodingMode' "CODING_MODE_1_0"

pattern AacCodingModeCodingMode11 :: AacCodingMode
pattern AacCodingModeCodingMode11 = AacCodingMode' "CODING_MODE_1_1"

pattern AacCodingModeCodingMode20 :: AacCodingMode
pattern AacCodingModeCodingMode20 = AacCodingMode' "CODING_MODE_2_0"

pattern AacCodingModeCodingMode51 :: AacCodingMode
pattern AacCodingModeCodingMode51 = AacCodingMode' "CODING_MODE_5_1"

{-# COMPLETE
  AacCodingModeAdReceiverMix,
  AacCodingModeCodingMode10,
  AacCodingModeCodingMode11,
  AacCodingModeCodingMode20,
  AacCodingModeCodingMode51,
  AacCodingMode'
  #-}
