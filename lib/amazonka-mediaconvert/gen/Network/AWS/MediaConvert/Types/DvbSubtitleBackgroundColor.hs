{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
  ( DvbSubtitleBackgroundColor
    ( DvbSubtitleBackgroundColor'
    , DvbSubtitleBackgroundColorNone
    , DvbSubtitleBackgroundColorBlack
    , DvbSubtitleBackgroundColorWhite
    , fromDvbSubtitleBackgroundColor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor'{fromDvbSubtitleBackgroundColor
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern DvbSubtitleBackgroundColorNone :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColorNone = DvbSubtitleBackgroundColor' "NONE"

pattern DvbSubtitleBackgroundColorBlack :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColorBlack = DvbSubtitleBackgroundColor' "BLACK"

pattern DvbSubtitleBackgroundColorWhite :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColorWhite = DvbSubtitleBackgroundColor' "WHITE"

{-# COMPLETE 
  DvbSubtitleBackgroundColorNone,

  DvbSubtitleBackgroundColorBlack,

  DvbSubtitleBackgroundColorWhite,
  DvbSubtitleBackgroundColor'
  #-}
