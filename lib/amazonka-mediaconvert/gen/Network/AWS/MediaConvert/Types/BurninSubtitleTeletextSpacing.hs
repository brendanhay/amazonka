{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        BurninSubtitleTeletextSpacingFixedGrid,
        BurninSubtitleTeletextSpacingProportional,
        fromBurninSubtitleTeletextSpacing
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
newtype BurninSubtitleTeletextSpacing = BurninSubtitleTeletextSpacing'
  { fromBurninSubtitleTeletextSpacing ::
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

pattern BurninSubtitleTeletextSpacingFixedGrid :: BurninSubtitleTeletextSpacing
pattern BurninSubtitleTeletextSpacingFixedGrid = BurninSubtitleTeletextSpacing' "FIXED_GRID"

pattern BurninSubtitleTeletextSpacingProportional :: BurninSubtitleTeletextSpacing
pattern BurninSubtitleTeletextSpacingProportional = BurninSubtitleTeletextSpacing' "PROPORTIONAL"

{-# COMPLETE
  BurninSubtitleTeletextSpacingFixedGrid,
  BurninSubtitleTeletextSpacingProportional,
  BurninSubtitleTeletextSpacing'
  #-}
