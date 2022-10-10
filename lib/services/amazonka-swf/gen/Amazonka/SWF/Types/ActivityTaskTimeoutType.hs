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
-- Module      : Amazonka.SWF.Types.ActivityTaskTimeoutType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTaskTimeoutType
  ( ActivityTaskTimeoutType
      ( ..,
        ActivityTaskTimeoutType_HEARTBEAT,
        ActivityTaskTimeoutType_SCHEDULE_TO_CLOSE,
        ActivityTaskTimeoutType_SCHEDULE_TO_START,
        ActivityTaskTimeoutType_START_TO_CLOSE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ActivityTaskTimeoutType = ActivityTaskTimeoutType'
  { fromActivityTaskTimeoutType ::
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

pattern ActivityTaskTimeoutType_HEARTBEAT :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutType_HEARTBEAT = ActivityTaskTimeoutType' "HEARTBEAT"

pattern ActivityTaskTimeoutType_SCHEDULE_TO_CLOSE :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutType_SCHEDULE_TO_CLOSE = ActivityTaskTimeoutType' "SCHEDULE_TO_CLOSE"

pattern ActivityTaskTimeoutType_SCHEDULE_TO_START :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutType_SCHEDULE_TO_START = ActivityTaskTimeoutType' "SCHEDULE_TO_START"

pattern ActivityTaskTimeoutType_START_TO_CLOSE :: ActivityTaskTimeoutType
pattern ActivityTaskTimeoutType_START_TO_CLOSE = ActivityTaskTimeoutType' "START_TO_CLOSE"

{-# COMPLETE
  ActivityTaskTimeoutType_HEARTBEAT,
  ActivityTaskTimeoutType_SCHEDULE_TO_CLOSE,
  ActivityTaskTimeoutType_SCHEDULE_TO_START,
  ActivityTaskTimeoutType_START_TO_CLOSE,
  ActivityTaskTimeoutType'
  #-}
