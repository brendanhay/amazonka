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
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleSortKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleSortKey
  ( MonitoringScheduleSortKey
      ( ..,
        MonitoringScheduleSortKey_CreationTime,
        MonitoringScheduleSortKey_Name,
        MonitoringScheduleSortKey_Status
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MonitoringScheduleSortKey = MonitoringScheduleSortKey'
  { fromMonitoringScheduleSortKey ::
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

pattern MonitoringScheduleSortKey_CreationTime :: MonitoringScheduleSortKey
pattern MonitoringScheduleSortKey_CreationTime = MonitoringScheduleSortKey' "CreationTime"

pattern MonitoringScheduleSortKey_Name :: MonitoringScheduleSortKey
pattern MonitoringScheduleSortKey_Name = MonitoringScheduleSortKey' "Name"

pattern MonitoringScheduleSortKey_Status :: MonitoringScheduleSortKey
pattern MonitoringScheduleSortKey_Status = MonitoringScheduleSortKey' "Status"

{-# COMPLETE
  MonitoringScheduleSortKey_CreationTime,
  MonitoringScheduleSortKey_Name,
  MonitoringScheduleSortKey_Status,
  MonitoringScheduleSortKey'
  #-}
