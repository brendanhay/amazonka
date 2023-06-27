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
-- Module      : Amazonka.QuickSight.Types.TopicScheduleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicScheduleType
  ( TopicScheduleType
      ( ..,
        TopicScheduleType_DAILY,
        TopicScheduleType_HOURLY,
        TopicScheduleType_MONTHLY,
        TopicScheduleType_WEEKLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TopicScheduleType = TopicScheduleType'
  { fromTopicScheduleType ::
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

pattern TopicScheduleType_DAILY :: TopicScheduleType
pattern TopicScheduleType_DAILY = TopicScheduleType' "DAILY"

pattern TopicScheduleType_HOURLY :: TopicScheduleType
pattern TopicScheduleType_HOURLY = TopicScheduleType' "HOURLY"

pattern TopicScheduleType_MONTHLY :: TopicScheduleType
pattern TopicScheduleType_MONTHLY = TopicScheduleType' "MONTHLY"

pattern TopicScheduleType_WEEKLY :: TopicScheduleType
pattern TopicScheduleType_WEEKLY = TopicScheduleType' "WEEKLY"

{-# COMPLETE
  TopicScheduleType_DAILY,
  TopicScheduleType_HOURLY,
  TopicScheduleType_MONTHLY,
  TopicScheduleType_WEEKLY,
  TopicScheduleType'
  #-}
