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
-- Module      : Amazonka.SageMaker.Types.MonitoringScheduleSortKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringScheduleSortKey
  ( MonitoringScheduleSortKey
      ( ..,
        MonitoringScheduleSortKey_CreationTime,
        MonitoringScheduleSortKey_Name,
        MonitoringScheduleSortKey_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MonitoringScheduleSortKey = MonitoringScheduleSortKey'
  { fromMonitoringScheduleSortKey ::
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
