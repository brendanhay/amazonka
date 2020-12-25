{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264CodecProfile
  ( H264CodecProfile
      ( H264CodecProfile',
        H264CodecProfileBaseline,
        H264CodecProfileHigh,
        H264CodecProfileHigh10BIT,
        H264CodecProfileHigh422,
        H264CodecProfileHigh42210BIT,
        H264CodecProfileMain,
        fromH264CodecProfile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
newtype H264CodecProfile = H264CodecProfile'
  { fromH264CodecProfile ::
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

pattern H264CodecProfileBaseline :: H264CodecProfile
pattern H264CodecProfileBaseline = H264CodecProfile' "BASELINE"

pattern H264CodecProfileHigh :: H264CodecProfile
pattern H264CodecProfileHigh = H264CodecProfile' "HIGH"

pattern H264CodecProfileHigh10BIT :: H264CodecProfile
pattern H264CodecProfileHigh10BIT = H264CodecProfile' "HIGH_10BIT"

pattern H264CodecProfileHigh422 :: H264CodecProfile
pattern H264CodecProfileHigh422 = H264CodecProfile' "HIGH_422"

pattern H264CodecProfileHigh42210BIT :: H264CodecProfile
pattern H264CodecProfileHigh42210BIT = H264CodecProfile' "HIGH_422_10BIT"

pattern H264CodecProfileMain :: H264CodecProfile
pattern H264CodecProfileMain = H264CodecProfile' "MAIN"

{-# COMPLETE
  H264CodecProfileBaseline,
  H264CodecProfileHigh,
  H264CodecProfileHigh10BIT,
  H264CodecProfileHigh422,
  H264CodecProfileHigh42210BIT,
  H264CodecProfileMain,
  H264CodecProfile'
  #-}
