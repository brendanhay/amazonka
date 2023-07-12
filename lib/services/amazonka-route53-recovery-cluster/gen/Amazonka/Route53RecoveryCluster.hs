{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53RecoveryCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the Routing Control (Recovery Cluster) API Reference Guide
-- for Amazon Route 53 Application Recovery Controller.
--
-- With Route 53 ARC, you can use routing control with extreme reliability
-- to recover applications by rerouting traffic across Availability Zones
-- or Amazon Web Services Regions. Routing controls are simple on\/off
-- switches hosted on a highly available cluster in Route 53 ARC. A cluster
-- provides a set of five redundant Regional endpoints against which you
-- can run API calls to get or update the state of routing controls. To
-- implement failover, you set one routing control On and another one Off,
-- to reroute traffic from one Availability Zone or Amazon Web Services
-- Region to another.
--
-- /Be aware that you must specify a Regional endpoint for a cluster when
-- you work with API cluster operations to get or update routing control
-- states in Route 53 ARC./ In addition, you must specify the US West
-- (Oregon) Region for Route 53 ARC API calls. For example, use the
-- parameter @--region us-west-2@ with AWS CLI commands. For more
-- information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.update.api.html Get and update routing control states using the API>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- This API guide includes information about the API operations for how to
-- get and update routing control states in Route 53 ARC. To work with
-- routing control in Route 53 ARC, you must first create the required
-- components (clusters, control panels, and routing controls) using the
-- recovery cluster configuration API.
--
-- For more information about working with routing control in Route 53 ARC,
-- see the following:
--
-- -   Create clusters, control panels, and routing controls by using API
--     operations. For more information, see the
--     <https://docs.aws.amazon.com/recovery-cluster/latest/api/ Recovery Control Configuration API Reference Guide for Amazon Route 53 Application Recovery Controller>.
--
-- -   Learn about the components in recovery control, including clusters,
--     routing controls, and control panels, and how to work with Route 53
--     ARC in the Amazon Web Services console. For more information, see
--     <https://docs.aws.amazon.com/r53recovery/latest/dg/introduction-components.html#introduction-components-routing Recovery control components>
--     in the Amazon Route 53 Application Recovery Controller Developer
--     Guide.
--
-- -   Route 53 ARC also provides readiness checks that continually audit
--     resources to help make sure that your applications are scaled and
--     ready to handle failover traffic. For more information about the
--     related API operations, see the
--     <https://docs.aws.amazon.com/recovery-readiness/latest/api/ Recovery Readiness API Reference Guide for Amazon Route 53 Application Recovery Controller>.
--
-- -   For more information about creating resilient applications and
--     preparing for recovery readiness with Route 53 ARC, see the
--     <https://docs.aws.amazon.com/r53recovery/latest/dg/ Amazon Route 53 Application Recovery Controller Developer Guide>.
module Amazonka.Route53RecoveryCluster
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** EndpointTemporarilyUnavailableException
    _EndpointTemporarilyUnavailableException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceLimitExceededException
    _ServiceLimitExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetRoutingControlState
    GetRoutingControlState (GetRoutingControlState'),
    newGetRoutingControlState,
    GetRoutingControlStateResponse (GetRoutingControlStateResponse'),
    newGetRoutingControlStateResponse,

    -- ** ListRoutingControls (Paginated)
    ListRoutingControls (ListRoutingControls'),
    newListRoutingControls,
    ListRoutingControlsResponse (ListRoutingControlsResponse'),
    newListRoutingControlsResponse,

    -- ** UpdateRoutingControlState
    UpdateRoutingControlState (UpdateRoutingControlState'),
    newUpdateRoutingControlState,
    UpdateRoutingControlStateResponse (UpdateRoutingControlStateResponse'),
    newUpdateRoutingControlStateResponse,

    -- ** UpdateRoutingControlStates
    UpdateRoutingControlStates (UpdateRoutingControlStates'),
    newUpdateRoutingControlStates,
    UpdateRoutingControlStatesResponse (UpdateRoutingControlStatesResponse'),
    newUpdateRoutingControlStatesResponse,

    -- * Types

    -- ** RoutingControlState
    RoutingControlState (..),

    -- ** RoutingControl
    RoutingControl (RoutingControl'),
    newRoutingControl,

    -- ** UpdateRoutingControlStateEntry
    UpdateRoutingControlStateEntry (UpdateRoutingControlStateEntry'),
    newUpdateRoutingControlStateEntry,
  )
where

import Amazonka.Route53RecoveryCluster.GetRoutingControlState
import Amazonka.Route53RecoveryCluster.Lens
import Amazonka.Route53RecoveryCluster.ListRoutingControls
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
