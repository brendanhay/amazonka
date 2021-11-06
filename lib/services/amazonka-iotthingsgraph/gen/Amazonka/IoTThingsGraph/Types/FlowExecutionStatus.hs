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
-- Module      : Amazonka.IoTThingsGraph.Types.FlowExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.FlowExecutionStatus
  ( FlowExecutionStatus
      ( ..,
        FlowExecutionStatus_ABORTED,
        FlowExecutionStatus_FAILED,
        FlowExecutionStatus_RUNNING,
        FlowExecutionStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FlowExecutionStatus = FlowExecutionStatus'
  { fromFlowExecutionStatus ::
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

pattern FlowExecutionStatus_ABORTED :: FlowExecutionStatus
pattern FlowExecutionStatus_ABORTED = FlowExecutionStatus' "ABORTED"

pattern FlowExecutionStatus_FAILED :: FlowExecutionStatus
pattern FlowExecutionStatus_FAILED = FlowExecutionStatus' "FAILED"

pattern FlowExecutionStatus_RUNNING :: FlowExecutionStatus
pattern FlowExecutionStatus_RUNNING = FlowExecutionStatus' "RUNNING"

pattern FlowExecutionStatus_SUCCEEDED :: FlowExecutionStatus
pattern FlowExecutionStatus_SUCCEEDED = FlowExecutionStatus' "SUCCEEDED"

{-# COMPLETE
  FlowExecutionStatus_ABORTED,
  FlowExecutionStatus_FAILED,
  FlowExecutionStatus_RUNNING,
  FlowExecutionStatus_SUCCEEDED,
  FlowExecutionStatus'
  #-}
