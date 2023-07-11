{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- With License Manager, you can discover and track your commercial Linux
-- subscriptions on running Amazon EC2 instances.
module Amazonka.LicenseManagerLinuxSubscriptions
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerException
    _InternalServerException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetServiceSettings
    GetServiceSettings (GetServiceSettings'),
    newGetServiceSettings,
    GetServiceSettingsResponse (GetServiceSettingsResponse'),
    newGetServiceSettingsResponse,

    -- ** ListLinuxSubscriptionInstances (Paginated)
    ListLinuxSubscriptionInstances (ListLinuxSubscriptionInstances'),
    newListLinuxSubscriptionInstances,
    ListLinuxSubscriptionInstancesResponse (ListLinuxSubscriptionInstancesResponse'),
    newListLinuxSubscriptionInstancesResponse,

    -- ** ListLinuxSubscriptions (Paginated)
    ListLinuxSubscriptions (ListLinuxSubscriptions'),
    newListLinuxSubscriptions,
    ListLinuxSubscriptionsResponse (ListLinuxSubscriptionsResponse'),
    newListLinuxSubscriptionsResponse,

    -- ** UpdateServiceSettings
    UpdateServiceSettings (UpdateServiceSettings'),
    newUpdateServiceSettings,
    UpdateServiceSettingsResponse (UpdateServiceSettingsResponse'),
    newUpdateServiceSettingsResponse,

    -- * Types

    -- ** LinuxSubscriptionsDiscovery
    LinuxSubscriptionsDiscovery (..),

    -- ** Operator
    Operator (..),

    -- ** OrganizationIntegration
    OrganizationIntegration (..),

    -- ** Status
    Status (..),

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** LinuxSubscriptionsDiscoverySettings
    LinuxSubscriptionsDiscoverySettings (LinuxSubscriptionsDiscoverySettings'),
    newLinuxSubscriptionsDiscoverySettings,

    -- ** Subscription
    Subscription (Subscription'),
    newSubscription,
  )
where

import Amazonka.LicenseManagerLinuxSubscriptions.GetServiceSettings
import Amazonka.LicenseManagerLinuxSubscriptions.Lens
import Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptionInstances
import Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptions
import Amazonka.LicenseManagerLinuxSubscriptions.Types
import Amazonka.LicenseManagerLinuxSubscriptions.UpdateServiceSettings
import Amazonka.LicenseManagerLinuxSubscriptions.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LicenseManagerLinuxSubscriptions'.

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
