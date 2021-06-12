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
-- Module      : Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
  ( VideoTimecodeInsertion
      ( ..,
        VideoTimecodeInsertion_DISABLED,
        VideoTimecodeInsertion_PIC_TIMING_SEI
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable
-- Timecode insertion when the input frame rate is identical to the output
-- frame rate. To include timecodes in this output, set Timecode insertion
-- (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to
-- DISABLED. Default is DISABLED. When the service inserts timecodes in an
-- output, by default, it uses any embedded timecodes from the input. If
-- none are present, the service will set the timecode for the first output
-- frame to zero. To change this default behavior, adjust the settings
-- under Timecode configuration (TimecodeConfig). In the console, these
-- settings are located under Job > Job settings > Timecode configuration.
-- Note - Timecode source under input settings (InputTimecodeSource) does
-- not affect the timecodes that are inserted in the output. Source under
-- Job settings > Timecode configuration (TimecodeSource) does.
newtype VideoTimecodeInsertion = VideoTimecodeInsertion'
  { fromVideoTimecodeInsertion ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern VideoTimecodeInsertion_DISABLED :: VideoTimecodeInsertion
pattern VideoTimecodeInsertion_DISABLED = VideoTimecodeInsertion' "DISABLED"

pattern VideoTimecodeInsertion_PIC_TIMING_SEI :: VideoTimecodeInsertion
pattern VideoTimecodeInsertion_PIC_TIMING_SEI = VideoTimecodeInsertion' "PIC_TIMING_SEI"

{-# COMPLETE
  VideoTimecodeInsertion_DISABLED,
  VideoTimecodeInsertion_PIC_TIMING_SEI,
  VideoTimecodeInsertion'
  #-}
