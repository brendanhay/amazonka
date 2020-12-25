{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TimecodeConfigSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TimecodeConfigSource
  ( TimecodeConfigSource
      ( TimecodeConfigSource',
        TimecodeConfigSourceEmbedded,
        TimecodeConfigSourceSystemclock,
        TimecodeConfigSourceZerobased,
        fromTimecodeConfigSource
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Timecode Config Source
newtype TimecodeConfigSource = TimecodeConfigSource'
  { fromTimecodeConfigSource ::
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

pattern TimecodeConfigSourceEmbedded :: TimecodeConfigSource
pattern TimecodeConfigSourceEmbedded = TimecodeConfigSource' "EMBEDDED"

pattern TimecodeConfigSourceSystemclock :: TimecodeConfigSource
pattern TimecodeConfigSourceSystemclock = TimecodeConfigSource' "SYSTEMCLOCK"

pattern TimecodeConfigSourceZerobased :: TimecodeConfigSource
pattern TimecodeConfigSourceZerobased = TimecodeConfigSource' "ZEROBASED"

{-# COMPLETE
  TimecodeConfigSourceEmbedded,
  TimecodeConfigSourceSystemclock,
  TimecodeConfigSourceZerobased,
  TimecodeConfigSource'
  #-}
