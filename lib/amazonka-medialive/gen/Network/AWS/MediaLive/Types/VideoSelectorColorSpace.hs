{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorColorSpace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorColorSpace
  ( VideoSelectorColorSpace
      ( VideoSelectorColorSpace',
        VideoSelectorColorSpaceFollow,
        VideoSelectorColorSpaceRec601,
        VideoSelectorColorSpaceRec709,
        fromVideoSelectorColorSpace
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Video Selector Color Space
newtype VideoSelectorColorSpace = VideoSelectorColorSpace'
  { fromVideoSelectorColorSpace ::
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

pattern VideoSelectorColorSpaceFollow :: VideoSelectorColorSpace
pattern VideoSelectorColorSpaceFollow = VideoSelectorColorSpace' "FOLLOW"

pattern VideoSelectorColorSpaceRec601 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpaceRec601 = VideoSelectorColorSpace' "REC_601"

pattern VideoSelectorColorSpaceRec709 :: VideoSelectorColorSpace
pattern VideoSelectorColorSpaceRec709 = VideoSelectorColorSpace' "REC_709"

{-# COMPLETE
  VideoSelectorColorSpaceFollow,
  VideoSelectorColorSpaceRec601,
  VideoSelectorColorSpaceRec709,
  VideoSelectorColorSpace'
  #-}
