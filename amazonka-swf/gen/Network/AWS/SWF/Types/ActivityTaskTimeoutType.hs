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
-- Module      : Network.AWS.SWF.Types.ActivityTaskTimeoutType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskTimeoutType
  ( ActivityTaskTimeoutType
      ( ..,
        ActivityTaskTimeoutType_HEARTBEAT,
        ActivityTaskTimeoutType_SCHEDULE_TO_CLOSE,
        ActivityTaskTimeoutType_SCHEDULE_TO_START,
        ActivityTaskTimeoutType_START_TO_CLOSE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActivityTaskTimeoutType = ActivityTaskTimeoutType'
  { fromActivityTaskTimeoutType ::
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
