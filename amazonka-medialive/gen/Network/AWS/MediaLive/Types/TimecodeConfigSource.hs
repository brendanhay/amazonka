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
-- Module      : Network.AWS.MediaLive.Types.TimecodeConfigSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TimecodeConfigSource
  ( TimecodeConfigSource
      ( ..,
        TimecodeConfigSource_EMBEDDED,
        TimecodeConfigSource_SYSTEMCLOCK,
        TimecodeConfigSource_ZEROBASED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Timecode Config Source
newtype TimecodeConfigSource = TimecodeConfigSource'
  { fromTimecodeConfigSource ::
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

pattern TimecodeConfigSource_EMBEDDED :: TimecodeConfigSource
pattern TimecodeConfigSource_EMBEDDED = TimecodeConfigSource' "EMBEDDED"

pattern TimecodeConfigSource_SYSTEMCLOCK :: TimecodeConfigSource
pattern TimecodeConfigSource_SYSTEMCLOCK = TimecodeConfigSource' "SYSTEMCLOCK"

pattern TimecodeConfigSource_ZEROBASED :: TimecodeConfigSource
pattern TimecodeConfigSource_ZEROBASED = TimecodeConfigSource' "ZEROBASED"

{-# COMPLETE
  TimecodeConfigSource_EMBEDDED,
  TimecodeConfigSource_SYSTEMCLOCK,
  TimecodeConfigSource_ZEROBASED,
  TimecodeConfigSource'
  #-}
