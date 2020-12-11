-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleTeletextSpacing
  ( BurninSubtitleTeletextSpacing
      ( BurninSubtitleTeletextSpacing',
        BSTSFixedGrid,
        BSTSProportional
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
newtype BurninSubtitleTeletextSpacing = BurninSubtitleTeletextSpacing' Lude.Text
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

pattern BSTSFixedGrid :: BurninSubtitleTeletextSpacing
pattern BSTSFixedGrid = BurninSubtitleTeletextSpacing' "FIXED_GRID"

pattern BSTSProportional :: BurninSubtitleTeletextSpacing
pattern BSTSProportional = BurninSubtitleTeletextSpacing' "PROPORTIONAL"

{-# COMPLETE
  BSTSFixedGrid,
  BSTSProportional,
  BurninSubtitleTeletextSpacing'
  #-}
