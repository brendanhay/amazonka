{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.LastFrameClippingBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.LastFrameClippingBehavior
  ( LastFrameClippingBehavior
      ( LastFrameClippingBehavior',
        LastFrameClippingBehaviorExcludeLastFrame,
        LastFrameClippingBehaviorIncludeLastFrame,
        fromLastFrameClippingBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
newtype LastFrameClippingBehavior = LastFrameClippingBehavior'
  { fromLastFrameClippingBehavior ::
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

pattern LastFrameClippingBehaviorExcludeLastFrame :: LastFrameClippingBehavior
pattern LastFrameClippingBehaviorExcludeLastFrame = LastFrameClippingBehavior' "EXCLUDE_LAST_FRAME"

pattern LastFrameClippingBehaviorIncludeLastFrame :: LastFrameClippingBehavior
pattern LastFrameClippingBehaviorIncludeLastFrame = LastFrameClippingBehavior' "INCLUDE_LAST_FRAME"

{-# COMPLETE
  LastFrameClippingBehaviorExcludeLastFrame,
  LastFrameClippingBehaviorIncludeLastFrame,
  LastFrameClippingBehavior'
  #-}
