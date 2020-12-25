{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265CodecProfile
  ( H265CodecProfile
      ( H265CodecProfile',
        H265CodecProfileMainMain,
        H265CodecProfileMainHigh,
        H265CodecProfileMAIN10Main,
        H265CodecProfileMAIN10High,
        H265CodecProfileMain4228BITMain,
        H265CodecProfileMain4228BITHigh,
        H265CodecProfileMain42210BITMain,
        H265CodecProfileMain42210BITHigh,
        fromH265CodecProfile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
newtype H265CodecProfile = H265CodecProfile'
  { fromH265CodecProfile ::
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

pattern H265CodecProfileMainMain :: H265CodecProfile
pattern H265CodecProfileMainMain = H265CodecProfile' "MAIN_MAIN"

pattern H265CodecProfileMainHigh :: H265CodecProfile
pattern H265CodecProfileMainHigh = H265CodecProfile' "MAIN_HIGH"

pattern H265CodecProfileMAIN10Main :: H265CodecProfile
pattern H265CodecProfileMAIN10Main = H265CodecProfile' "MAIN10_MAIN"

pattern H265CodecProfileMAIN10High :: H265CodecProfile
pattern H265CodecProfileMAIN10High = H265CodecProfile' "MAIN10_HIGH"

pattern H265CodecProfileMain4228BITMain :: H265CodecProfile
pattern H265CodecProfileMain4228BITMain = H265CodecProfile' "MAIN_422_8BIT_MAIN"

pattern H265CodecProfileMain4228BITHigh :: H265CodecProfile
pattern H265CodecProfileMain4228BITHigh = H265CodecProfile' "MAIN_422_8BIT_HIGH"

pattern H265CodecProfileMain42210BITMain :: H265CodecProfile
pattern H265CodecProfileMain42210BITMain = H265CodecProfile' "MAIN_422_10BIT_MAIN"

pattern H265CodecProfileMain42210BITHigh :: H265CodecProfile
pattern H265CodecProfileMain42210BITHigh = H265CodecProfile' "MAIN_422_10BIT_HIGH"

{-# COMPLETE
  H265CodecProfileMainMain,
  H265CodecProfileMainHigh,
  H265CodecProfileMAIN10Main,
  H265CodecProfileMAIN10High,
  H265CodecProfileMain4228BITMain,
  H265CodecProfileMain4228BITHigh,
  H265CodecProfileMain42210BITMain,
  H265CodecProfileMain42210BITHigh,
  H265CodecProfile'
  #-}
