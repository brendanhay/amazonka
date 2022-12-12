{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryCluster.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryCluster.Lens
  ( -- * Operations

    -- ** GetRoutingControlState
    getRoutingControlState_routingControlArn,
    getRoutingControlStateResponse_routingControlName,
    getRoutingControlStateResponse_httpStatus,
    getRoutingControlStateResponse_routingControlArn,
    getRoutingControlStateResponse_routingControlState,

    -- ** ListRoutingControls
    listRoutingControls_controlPanelArn,
    listRoutingControls_maxResults,
    listRoutingControls_nextToken,
    listRoutingControlsResponse_nextToken,
    listRoutingControlsResponse_httpStatus,
    listRoutingControlsResponse_routingControls,

    -- ** UpdateRoutingControlState
    updateRoutingControlState_safetyRulesToOverride,
    updateRoutingControlState_routingControlArn,
    updateRoutingControlState_routingControlState,
    updateRoutingControlStateResponse_httpStatus,

    -- ** UpdateRoutingControlStates
    updateRoutingControlStates_safetyRulesToOverride,
    updateRoutingControlStates_updateRoutingControlStateEntries,
    updateRoutingControlStatesResponse_httpStatus,

    -- * Types

    -- ** RoutingControl
    routingControl_controlPanelArn,
    routingControl_controlPanelName,
    routingControl_routingControlArn,
    routingControl_routingControlName,
    routingControl_routingControlState,

    -- ** UpdateRoutingControlStateEntry
    updateRoutingControlStateEntry_routingControlArn,
    updateRoutingControlStateEntry_routingControlState,
  )
where

import Amazonka.Route53RecoveryCluster.GetRoutingControlState
import Amazonka.Route53RecoveryCluster.ListRoutingControls
import Amazonka.Route53RecoveryCluster.Types.RoutingControl
import Amazonka.Route53RecoveryCluster.Types.UpdateRoutingControlStateEntry
import Amazonka.Route53RecoveryCluster.UpdateRoutingControlState
import Amazonka.Route53RecoveryCluster.UpdateRoutingControlStates
