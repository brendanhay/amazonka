{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeSource
  ( TimecodeSource
      ( ..,
        TimecodeSource_EMBEDDED,
        TimecodeSource_SPECIFIEDSTART,
        TimecodeSource_ZEROBASED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Source (TimecodeSource) to set how timecodes are handled within this
-- job. To make sure that your video, audio, captions, and markers are
-- synchronized and that time-based features, such as image inserter, work
-- correctly, choose the Timecode source option that matches your assets.
-- All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). *
-- Embedded (EMBEDDED) - Use the timecode that is in the input video. If no
-- embedded timecode is in the source, the service will use Start at 0
-- (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the
-- initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set
-- the timecode of the initial frame to a value other than zero. You use
-- Start timecode (Start) to provide this value.
newtype TimecodeSource = TimecodeSource'
  { fromTimecodeSource ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern TimecodeSource_EMBEDDED :: TimecodeSource
pattern TimecodeSource_EMBEDDED = TimecodeSource' "EMBEDDED"

pattern TimecodeSource_SPECIFIEDSTART :: TimecodeSource
pattern TimecodeSource_SPECIFIEDSTART = TimecodeSource' "SPECIFIEDSTART"

pattern TimecodeSource_ZEROBASED :: TimecodeSource
pattern TimecodeSource_ZEROBASED = TimecodeSource' "ZEROBASED"

{-# COMPLETE
  TimecodeSource_EMBEDDED,
  TimecodeSource_SPECIFIEDSTART,
  TimecodeSource_ZEROBASED,
  TimecodeSource'
  #-}
