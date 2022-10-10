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
-- Module      : Amazonka.MediaLive.Types.HlsProgramDateTimeClock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsProgramDateTimeClock
  ( HlsProgramDateTimeClock
      ( ..,
        HlsProgramDateTimeClock_INITIALIZE_FROM_OUTPUT_TIMECODE,
        HlsProgramDateTimeClock_SYSTEM_CLOCK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Hls Program Date Time Clock
newtype HlsProgramDateTimeClock = HlsProgramDateTimeClock'
  { fromHlsProgramDateTimeClock ::
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

pattern HlsProgramDateTimeClock_INITIALIZE_FROM_OUTPUT_TIMECODE :: HlsProgramDateTimeClock
pattern HlsProgramDateTimeClock_INITIALIZE_FROM_OUTPUT_TIMECODE = HlsProgramDateTimeClock' "INITIALIZE_FROM_OUTPUT_TIMECODE"

pattern HlsProgramDateTimeClock_SYSTEM_CLOCK :: HlsProgramDateTimeClock
pattern HlsProgramDateTimeClock_SYSTEM_CLOCK = HlsProgramDateTimeClock' "SYSTEM_CLOCK"

{-# COMPLETE
  HlsProgramDateTimeClock_INITIALIZE_FROM_OUTPUT_TIMECODE,
  HlsProgramDateTimeClock_SYSTEM_CLOCK,
  HlsProgramDateTimeClock'
  #-}
