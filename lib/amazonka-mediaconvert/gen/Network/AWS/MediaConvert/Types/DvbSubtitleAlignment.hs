{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleAlignment
  ( DvbSubtitleAlignment
      ( DvbSubtitleAlignment',
        DvbSubtitleAlignmentCentered,
        DvbSubtitleAlignmentLeft,
        fromDvbSubtitleAlignment
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleAlignment = DvbSubtitleAlignment'
  { fromDvbSubtitleAlignment ::
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

pattern DvbSubtitleAlignmentCentered :: DvbSubtitleAlignment
pattern DvbSubtitleAlignmentCentered = DvbSubtitleAlignment' "CENTERED"

pattern DvbSubtitleAlignmentLeft :: DvbSubtitleAlignment
pattern DvbSubtitleAlignmentLeft = DvbSubtitleAlignment' "LEFT"

{-# COMPLETE
  DvbSubtitleAlignmentCentered,
  DvbSubtitleAlignmentLeft,
  DvbSubtitleAlignment'
  #-}
