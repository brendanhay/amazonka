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
-- Module      : Amazonka.SageMaker.Types.MonitoringExecutionSortKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringExecutionSortKey
  ( MonitoringExecutionSortKey
      ( ..,
        MonitoringExecutionSortKey_CreationTime,
        MonitoringExecutionSortKey_ScheduledTime,
        MonitoringExecutionSortKey_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MonitoringExecutionSortKey = MonitoringExecutionSortKey'
  { fromMonitoringExecutionSortKey ::
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

pattern MonitoringExecutionSortKey_CreationTime :: MonitoringExecutionSortKey
pattern MonitoringExecutionSortKey_CreationTime = MonitoringExecutionSortKey' "CreationTime"

pattern MonitoringExecutionSortKey_ScheduledTime :: MonitoringExecutionSortKey
pattern MonitoringExecutionSortKey_ScheduledTime = MonitoringExecutionSortKey' "ScheduledTime"

pattern MonitoringExecutionSortKey_Status :: MonitoringExecutionSortKey
pattern MonitoringExecutionSortKey_Status = MonitoringExecutionSortKey' "Status"

{-# COMPLETE
  MonitoringExecutionSortKey_CreationTime,
  MonitoringExecutionSortKey_ScheduledTime,
  MonitoringExecutionSortKey_Status,
  MonitoringExecutionSortKey'
  #-}
