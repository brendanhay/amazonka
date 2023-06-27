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
-- Module      : Amazonka.InternetMonitor.Types.MonitorProcessingStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.MonitorProcessingStatusCode
  ( MonitorProcessingStatusCode
      ( ..,
        MonitorProcessingStatusCode_COLLECTING_DATA,
        MonitorProcessingStatusCode_FAULT_ACCESS_CLOUDWATCH,
        MonitorProcessingStatusCode_FAULT_SERVICE,
        MonitorProcessingStatusCode_INACTIVE,
        MonitorProcessingStatusCode_INSUFFICIENT_DATA,
        MonitorProcessingStatusCode_OK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MonitorProcessingStatusCode = MonitorProcessingStatusCode'
  { fromMonitorProcessingStatusCode ::
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

pattern MonitorProcessingStatusCode_COLLECTING_DATA :: MonitorProcessingStatusCode
pattern MonitorProcessingStatusCode_COLLECTING_DATA = MonitorProcessingStatusCode' "COLLECTING_DATA"

pattern MonitorProcessingStatusCode_FAULT_ACCESS_CLOUDWATCH :: MonitorProcessingStatusCode
pattern MonitorProcessingStatusCode_FAULT_ACCESS_CLOUDWATCH = MonitorProcessingStatusCode' "FAULT_ACCESS_CLOUDWATCH"

pattern MonitorProcessingStatusCode_FAULT_SERVICE :: MonitorProcessingStatusCode
pattern MonitorProcessingStatusCode_FAULT_SERVICE = MonitorProcessingStatusCode' "FAULT_SERVICE"

pattern MonitorProcessingStatusCode_INACTIVE :: MonitorProcessingStatusCode
pattern MonitorProcessingStatusCode_INACTIVE = MonitorProcessingStatusCode' "INACTIVE"

pattern MonitorProcessingStatusCode_INSUFFICIENT_DATA :: MonitorProcessingStatusCode
pattern MonitorProcessingStatusCode_INSUFFICIENT_DATA = MonitorProcessingStatusCode' "INSUFFICIENT_DATA"

pattern MonitorProcessingStatusCode_OK :: MonitorProcessingStatusCode
pattern MonitorProcessingStatusCode_OK = MonitorProcessingStatusCode' "OK"

{-# COMPLETE
  MonitorProcessingStatusCode_COLLECTING_DATA,
  MonitorProcessingStatusCode_FAULT_ACCESS_CLOUDWATCH,
  MonitorProcessingStatusCode_FAULT_SERVICE,
  MonitorProcessingStatusCode_INACTIVE,
  MonitorProcessingStatusCode_INSUFFICIENT_DATA,
  MonitorProcessingStatusCode_OK,
  MonitorProcessingStatusCode'
  #-}
