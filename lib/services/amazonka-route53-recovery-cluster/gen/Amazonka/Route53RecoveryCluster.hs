{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53RecoveryCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the Amazon Route 53 Application Recovery Controller API
-- Reference Guide for Recovery Control Data Plane .
--
-- Recovery control in Route 53 Application Recovery Controller includes
-- extremely reliable routing controls that enable you to recover
-- applications by rerouting traffic, for example, across Availability
-- Zones or AWS Regions. Routing controls are simple on\/off switches
-- hosted on a cluster. A cluster is a set of five redundant regional
-- endpoints against which you can execute API calls to update or get the
-- state of routing controls. You use routing controls to failover traffic
-- to recover your application across Availability Zones or Regions.
--
-- This API guide includes information about how to get and update routing
-- control states in Route 53 Application Recovery Controller.
--
-- For more information about Route 53 Application Recovery Controller, see
-- the following:
--
-- -   You can create clusters, routing controls, and control panels by
--     using the control plane API for Recovery Control. For more
--     information, see
--     <https://docs.aws.amazon.com/recovery-cluster/latest/api/ Amazon Route 53 Application Recovery Controller Recovery Control API Reference>.
--
-- -   Route 53 Application Recovery Controller also provides continuous
--     readiness checks to ensure that your applications are scaled to
--     handle failover traffic. For more information about the related API
--     actions, see
--     <https://docs.aws.amazon.com/recovery-readiness/latest/api/ Amazon Route 53 Application Recovery Controller Recovery Readiness API Reference>.
--
-- -   For more information about creating resilient applications and
--     preparing for recovery readiness with Route 53 Application Recovery
--     Controller, see the
--     <r53recovery/latest/dg/ Amazon Route 53 Application Recovery Controller Developer Guide>.
module Amazonka.Route53RecoveryCluster
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** EndpointTemporarilyUnavailableException
    _EndpointTemporarilyUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateRoutingControlState
    UpdateRoutingControlState (UpdateRoutingControlState'),
    newUpdateRoutingControlState,
    UpdateRoutingControlStateResponse (UpdateRoutingControlStateResponse'),
    newUpdateRoutingControlStateResponse,

    -- ** GetRoutingControlState
    GetRoutingControlState (GetRoutingControlState'),
    newGetRoutingControlState,
    GetRoutingControlStateResponse (GetRoutingControlStateResponse'),
    newGetRoutingControlStateResponse,

    -- ** UpdateRoutingControlStates
    UpdateRoutingControlStates (UpdateRoutingControlStates'),
    newUpdateRoutingControlStates,
    UpdateRoutingControlStatesResponse (UpdateRoutingControlStatesResponse'),
    newUpdateRoutingControlStatesResponse,

    -- * Types

    -- ** RoutingControlState
    RoutingControlState (..),

    -- ** UpdateRoutingControlStateEntry
    UpdateRoutingControlStateEntry (UpdateRoutingControlStateEntry'),
    newUpdateRoutingControlStateEntry,
  )
where

import Amazonka.Route53RecoveryCluster.GetRoutingControlState
import Amazonka.Route53RecoveryCluster.Lens
import Amazonka.Route53RecoveryCluster.Types
import Amazonka.Route53RecoveryCluster.UpdateRoutingControlState
import Amazonka.Route53RecoveryCluster.UpdateRoutingControlStates
import Amazonka.Route53RecoveryCluster.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53RecoveryCluster'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
