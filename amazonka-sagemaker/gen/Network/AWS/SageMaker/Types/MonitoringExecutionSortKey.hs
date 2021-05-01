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
-- Module      : Network.AWS.SageMaker.Types.MonitoringExecutionSortKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringExecutionSortKey
  ( MonitoringExecutionSortKey
      ( ..,
        MonitoringExecutionSortKey_CreationTime,
        MonitoringExecutionSortKey_ScheduledTime,
        MonitoringExecutionSortKey_Status
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MonitoringExecutionSortKey = MonitoringExecutionSortKey'
  { fromMonitoringExecutionSortKey ::
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
