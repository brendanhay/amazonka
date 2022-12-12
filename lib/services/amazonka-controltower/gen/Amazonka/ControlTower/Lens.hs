{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ControlTower.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ControlTower.Lens
  ( -- * Operations

    -- ** DisableControl
    disableControl_controlIdentifier,
    disableControl_targetIdentifier,
    disableControlResponse_httpStatus,
    disableControlResponse_operationIdentifier,

    -- ** EnableControl
    enableControl_controlIdentifier,
    enableControl_targetIdentifier,
    enableControlResponse_httpStatus,
    enableControlResponse_operationIdentifier,

    -- ** GetControlOperation
    getControlOperation_operationIdentifier,
    getControlOperationResponse_httpStatus,
    getControlOperationResponse_controlOperation,

    -- ** ListEnabledControls
    listEnabledControls_maxResults,
    listEnabledControls_nextToken,
    listEnabledControls_targetIdentifier,
    listEnabledControlsResponse_nextToken,
    listEnabledControlsResponse_httpStatus,
    listEnabledControlsResponse_enabledControls,

    -- * Types

    -- ** ControlOperation
    controlOperation_endTime,
    controlOperation_operationType,
    controlOperation_startTime,
    controlOperation_status,
    controlOperation_statusMessage,

    -- ** EnabledControlSummary
    enabledControlSummary_controlIdentifier,
  )
where

import Amazonka.ControlTower.DisableControl
import Amazonka.ControlTower.EnableControl
import Amazonka.ControlTower.GetControlOperation
import Amazonka.ControlTower.ListEnabledControls
import Amazonka.ControlTower.Types.ControlOperation
import Amazonka.ControlTower.Types.EnabledControlSummary
