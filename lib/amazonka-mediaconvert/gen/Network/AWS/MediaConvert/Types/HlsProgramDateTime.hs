{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsProgramDateTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsProgramDateTime
  ( HlsProgramDateTime
    ( HlsProgramDateTime'
    , HlsProgramDateTimeInclude
    , HlsProgramDateTimeExclude
    , fromHlsProgramDateTime
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
newtype HlsProgramDateTime = HlsProgramDateTime'{fromHlsProgramDateTime
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern HlsProgramDateTimeInclude :: HlsProgramDateTime
pattern HlsProgramDateTimeInclude = HlsProgramDateTime' "INCLUDE"

pattern HlsProgramDateTimeExclude :: HlsProgramDateTime
pattern HlsProgramDateTimeExclude = HlsProgramDateTime' "EXCLUDE"

{-# COMPLETE 
  HlsProgramDateTimeInclude,

  HlsProgramDateTimeExclude,
  HlsProgramDateTime'
  #-}
