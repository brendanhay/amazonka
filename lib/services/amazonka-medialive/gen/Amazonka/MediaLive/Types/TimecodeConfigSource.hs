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
-- Module      : Amazonka.MediaLive.Types.TimecodeConfigSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TimecodeConfigSource
  ( TimecodeConfigSource
      ( ..,
        TimecodeConfigSource_EMBEDDED,
        TimecodeConfigSource_SYSTEMCLOCK,
        TimecodeConfigSource_ZEROBASED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Timecode Config Source
newtype TimecodeConfigSource = TimecodeConfigSource'
  { fromTimecodeConfigSource ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
