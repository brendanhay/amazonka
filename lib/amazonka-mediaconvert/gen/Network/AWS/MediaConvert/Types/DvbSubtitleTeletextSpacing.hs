{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleTeletextSpacing
  ( DvbSubtitleTeletextSpacing
      ( DvbSubtitleTeletextSpacing',
        DvbSubtitleTeletextSpacingFixedGrid,
        DvbSubtitleTeletextSpacingProportional,
        fromDvbSubtitleTeletextSpacing
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Only applies to jobs with input captions in Teletext or STL formats. Specify whether the spacing between letters in your captions is set by the captions grid or varies depending on letter width. Choose fixed grid to conform to the spacing specified in the captions file more accurately. Choose proportional to make the text easier to read if the captions are closed caption.
newtype DvbSubtitleTeletextSpacing = DvbSubtitleTeletextSpacing'
  { fromDvbSubtitleTeletextSpacing ::
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

pattern DvbSubtitleTeletextSpacingFixedGrid :: DvbSubtitleTeletextSpacing
pattern DvbSubtitleTeletextSpacingFixedGrid = DvbSubtitleTeletextSpacing' "FIXED_GRID"

pattern DvbSubtitleTeletextSpacingProportional :: DvbSubtitleTeletextSpacing
pattern DvbSubtitleTeletextSpacingProportional = DvbSubtitleTeletextSpacing' "PROPORTIONAL"

{-# COMPLETE
  DvbSubtitleTeletextSpacingFixedGrid,
  DvbSubtitleTeletextSpacingProportional,
  DvbSubtitleTeletextSpacing'
  #-}
