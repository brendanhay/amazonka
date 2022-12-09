{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LicenseManagerUserSubscriptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- With License Manager, you can create user-based subscriptions to utilize
-- licensed software with a per user subscription fee on Amazon EC2
-- instances.
module Amazonka.LicenseManagerUserSubscriptions
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

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateUser
    AssociateUser (AssociateUser'),
    newAssociateUser,
    AssociateUserResponse (AssociateUserResponse'),
    newAssociateUserResponse,

    -- ** DeregisterIdentityProvider
    DeregisterIdentityProvider (DeregisterIdentityProvider'),
    newDeregisterIdentityProvider,
    DeregisterIdentityProviderResponse (DeregisterIdentityProviderResponse'),
    newDeregisterIdentityProviderResponse,

    -- ** DisassociateUser
    DisassociateUser (DisassociateUser'),
    newDisassociateUser,
    DisassociateUserResponse (DisassociateUserResponse'),
    newDisassociateUserResponse,

    -- ** ListIdentityProviders (Paginated)
    ListIdentityProviders (ListIdentityProviders'),
    newListIdentityProviders,
    ListIdentityProvidersResponse (ListIdentityProvidersResponse'),
    newListIdentityProvidersResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** ListProductSubscriptions (Paginated)
    ListProductSubscriptions (ListProductSubscriptions'),
    newListProductSubscriptions,
    ListProductSubscriptionsResponse (ListProductSubscriptionsResponse'),
    newListProductSubscriptionsResponse,

    -- ** ListUserAssociations (Paginated)
    ListUserAssociations (ListUserAssociations'),
    newListUserAssociations,
    ListUserAssociationsResponse (ListUserAssociationsResponse'),
    newListUserAssociationsResponse,

    -- ** RegisterIdentityProvider
    RegisterIdentityProvider (RegisterIdentityProvider'),
    newRegisterIdentityProvider,
    RegisterIdentityProviderResponse (RegisterIdentityProviderResponse'),
    newRegisterIdentityProviderResponse,

    -- ** StartProductSubscription
    StartProductSubscription (StartProductSubscription'),
    newStartProductSubscription,
    StartProductSubscriptionResponse (StartProductSubscriptionResponse'),
    newStartProductSubscriptionResponse,

    -- ** StopProductSubscription
    StopProductSubscription (StopProductSubscription'),
    newStopProductSubscription,
    StopProductSubscriptionResponse (StopProductSubscriptionResponse'),
    newStopProductSubscriptionResponse,

    -- ** UpdateIdentityProviderSettings
    UpdateIdentityProviderSettings (UpdateIdentityProviderSettings'),
    newUpdateIdentityProviderSettings,
    UpdateIdentityProviderSettingsResponse (UpdateIdentityProviderSettingsResponse'),
    newUpdateIdentityProviderSettingsResponse,

    -- * Types

    -- ** ActiveDirectoryIdentityProvider
    ActiveDirectoryIdentityProvider (ActiveDirectoryIdentityProvider'),
    newActiveDirectoryIdentityProvider,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** IdentityProvider
    IdentityProvider (IdentityProvider'),
    newIdentityProvider,

    -- ** IdentityProviderSummary
    IdentityProviderSummary (IdentityProviderSummary'),
    newIdentityProviderSummary,

    -- ** InstanceSummary
    InstanceSummary (InstanceSummary'),
    newInstanceSummary,

    -- ** InstanceUserSummary
    InstanceUserSummary (InstanceUserSummary'),
    newInstanceUserSummary,

    -- ** ProductUserSummary
    ProductUserSummary (ProductUserSummary'),
    newProductUserSummary,

    -- ** Settings
    Settings (Settings'),
    newSettings,

    -- ** UpdateSettings
    UpdateSettings (UpdateSettings'),
    newUpdateSettings,
  )
where

import Amazonka.LicenseManagerUserSubscriptions.AssociateUser
import Amazonka.LicenseManagerUserSubscriptions.DeregisterIdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.DisassociateUser
import Amazonka.LicenseManagerUserSubscriptions.Lens
import Amazonka.LicenseManagerUserSubscriptions.ListIdentityProviders
import Amazonka.LicenseManagerUserSubscriptions.ListInstances
import Amazonka.LicenseManagerUserSubscriptions.ListProductSubscriptions
import Amazonka.LicenseManagerUserSubscriptions.ListUserAssociations
import Amazonka.LicenseManagerUserSubscriptions.RegisterIdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.StartProductSubscription
import Amazonka.LicenseManagerUserSubscriptions.StopProductSubscription
import Amazonka.LicenseManagerUserSubscriptions.Types
import Amazonka.LicenseManagerUserSubscriptions.UpdateIdentityProviderSettings
import Amazonka.LicenseManagerUserSubscriptions.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LicenseManagerUserSubscriptions'.

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
