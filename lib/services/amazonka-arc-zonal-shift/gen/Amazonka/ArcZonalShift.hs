{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ArcZonalShift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-10-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the API Reference Guide for the zonal shift feature of Amazon
-- Route 53 Application Recovery Controller. This guide is for developers
-- who need detailed information about zonal shift API actions, data types,
-- and errors.
--
-- Zonal shift is in preview release for Amazon Route 53 Application
-- Recovery Controller and is subject to change.
--
-- Zonal shift in Route 53 ARC enables you to move traffic for a load
-- balancer resource away from an Availability Zone. Starting a zonal shift
-- helps your application recover immediately, for example, from a
-- developer\'s bad code deployment or from an AWS infrastructure failure
-- in a single Availability Zone, reducing the impact and time lost from an
-- issue in one zone.
--
-- Supported AWS resources are automatically registered with Route 53 ARC.
-- Resources that are registered for zonal shifts in Route 53 ARC are
-- managed resources in Route 53 ARC. You can start a zonal shift for any
-- managed resource in your account in a Region. At this time, you can only
-- start a zonal shift for Network Load Balancers and Application Load
-- Balancers with cross-zone load balancing turned off.
--
-- Zonal shifts are temporary. You must specify an expiration when you
-- start a zonal shift, of up to three days initially. If you want to still
-- keep traffic away from an Availability Zone, you can update the zonal
-- shift and set a new expiration. You can also cancel a zonal shift,
-- before it expires, for example, if you\'re ready to restore traffic to
-- the Availability Zone.
--
-- For more information about using zonal shift, see the
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/what-is-route53-recovery.html Amazon Route 53 Application Recovery Controller Developer Guide>.
module Amazonka.ArcZonalShift
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelZonalShift
    CancelZonalShift (CancelZonalShift'),
    newCancelZonalShift,
    ZonalShift (ZonalShift'),
    newZonalShift,

    -- ** GetManagedResource
    GetManagedResource (GetManagedResource'),
    newGetManagedResource,
    GetManagedResourceResponse (GetManagedResourceResponse'),
    newGetManagedResourceResponse,

    -- ** ListManagedResources (Paginated)
    ListManagedResources (ListManagedResources'),
    newListManagedResources,
    ListManagedResourcesResponse (ListManagedResourcesResponse'),
    newListManagedResourcesResponse,

    -- ** ListZonalShifts (Paginated)
    ListZonalShifts (ListZonalShifts'),
    newListZonalShifts,
    ListZonalShiftsResponse (ListZonalShiftsResponse'),
    newListZonalShiftsResponse,

    -- ** StartZonalShift
    StartZonalShift (StartZonalShift'),
    newStartZonalShift,
    ZonalShift (ZonalShift'),
    newZonalShift,

    -- ** UpdateZonalShift
    UpdateZonalShift (UpdateZonalShift'),
    newUpdateZonalShift,
    ZonalShift (ZonalShift'),
    newZonalShift,

    -- * Types

    -- ** AppliedStatus
    AppliedStatus (..),

    -- ** ZonalShiftStatus
    ZonalShiftStatus (..),

    -- ** ManagedResourceSummary
    ManagedResourceSummary (ManagedResourceSummary'),
    newManagedResourceSummary,

    -- ** ZonalShift
    ZonalShift (ZonalShift'),
    newZonalShift,

    -- ** ZonalShiftInResource
    ZonalShiftInResource (ZonalShiftInResource'),
    newZonalShiftInResource,

    -- ** ZonalShiftSummary
    ZonalShiftSummary (ZonalShiftSummary'),
    newZonalShiftSummary,
  )
where

import Amazonka.ArcZonalShift.CancelZonalShift
import Amazonka.ArcZonalShift.GetManagedResource
import Amazonka.ArcZonalShift.Lens
import Amazonka.ArcZonalShift.ListManagedResources
import Amazonka.ArcZonalShift.ListZonalShifts
import Amazonka.ArcZonalShift.StartZonalShift
import Amazonka.ArcZonalShift.Types
import Amazonka.ArcZonalShift.UpdateZonalShift
import Amazonka.ArcZonalShift.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ArcZonalShift'.

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
