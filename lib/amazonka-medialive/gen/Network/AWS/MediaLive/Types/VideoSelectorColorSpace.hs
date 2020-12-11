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
        Follow,
        Rec601,
        Rec709
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Video Selector Color Space
newtype VideoSelectorColorSpace = VideoSelectorColorSpace' Lude.Text
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

pattern Follow :: VideoSelectorColorSpace
pattern Follow = VideoSelectorColorSpace' "FOLLOW"

pattern Rec601 :: VideoSelectorColorSpace
pattern Rec601 = VideoSelectorColorSpace' "REC_601"

pattern Rec709 :: VideoSelectorColorSpace
pattern Rec709 = VideoSelectorColorSpace' "REC_709"

{-# COMPLETE
  Follow,
  Rec601,
  Rec709,
  VideoSelectorColorSpace'
  #-}
