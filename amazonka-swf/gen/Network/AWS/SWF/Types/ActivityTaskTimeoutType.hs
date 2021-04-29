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

import qualified Network.AWS.Prelude as Prelude

newtype ActivityTaskTimeoutType = ActivityTaskTimeoutType'
  { fromActivityTaskTimeoutType ::
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
