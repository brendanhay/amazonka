{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Timecode Config Source
newtype TimecodeConfigSource = TimecodeConfigSource'
  { fromTimecodeConfigSource ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
