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
        ExcludeLastFrame,
        IncludeLastFrame
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
newtype LastFrameClippingBehavior = LastFrameClippingBehavior' Lude.Text
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

pattern ExcludeLastFrame :: LastFrameClippingBehavior
pattern ExcludeLastFrame = LastFrameClippingBehavior' "EXCLUDE_LAST_FRAME"

pattern IncludeLastFrame :: LastFrameClippingBehavior
pattern IncludeLastFrame = LastFrameClippingBehavior' "INCLUDE_LAST_FRAME"

{-# COMPLETE
  ExcludeLastFrame,
  IncludeLastFrame,
  LastFrameClippingBehavior'
  #-}
