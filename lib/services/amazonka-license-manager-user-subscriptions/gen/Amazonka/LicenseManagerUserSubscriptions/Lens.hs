{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Lens
  ( -- * Operations

    -- ** AssociateUser
    associateUser_domain,
    associateUser_identityProvider,
    associateUser_instanceId,
    associateUser_username,
    associateUserResponse_httpStatus,
    associateUserResponse_instanceUserSummary,

    -- ** DeregisterIdentityProvider
    deregisterIdentityProvider_identityProvider,
    deregisterIdentityProvider_product,
    deregisterIdentityProviderResponse_httpStatus,
    deregisterIdentityProviderResponse_identityProviderSummary,

    -- ** DisassociateUser
    disassociateUser_domain,
    disassociateUser_identityProvider,
    disassociateUser_instanceId,
    disassociateUser_username,
    disassociateUserResponse_httpStatus,
    disassociateUserResponse_instanceUserSummary,

    -- ** ListIdentityProviders
    listIdentityProviders_maxResults,
    listIdentityProviders_nextToken,
    listIdentityProvidersResponse_nextToken,
    listIdentityProvidersResponse_httpStatus,
    listIdentityProvidersResponse_identityProviderSummaries,

    -- ** ListInstances
    listInstances_filters,
    listInstances_maxResults,
    listInstances_nextToken,
    listInstancesResponse_instanceSummaries,
    listInstancesResponse_nextToken,
    listInstancesResponse_httpStatus,

    -- ** ListProductSubscriptions
    listProductSubscriptions_filters,
    listProductSubscriptions_maxResults,
    listProductSubscriptions_nextToken,
    listProductSubscriptions_identityProvider,
    listProductSubscriptions_product,
    listProductSubscriptionsResponse_nextToken,
    listProductSubscriptionsResponse_productUserSummaries,
    listProductSubscriptionsResponse_httpStatus,

    -- ** ListUserAssociations
    listUserAssociations_filters,
    listUserAssociations_maxResults,
    listUserAssociations_nextToken,
    listUserAssociations_identityProvider,
    listUserAssociations_instanceId,
    listUserAssociationsResponse_instanceUserSummaries,
    listUserAssociationsResponse_nextToken,
    listUserAssociationsResponse_httpStatus,

    -- ** RegisterIdentityProvider
    registerIdentityProvider_settings,
    registerIdentityProvider_identityProvider,
    registerIdentityProvider_product,
    registerIdentityProviderResponse_httpStatus,
    registerIdentityProviderResponse_identityProviderSummary,

    -- ** StartProductSubscription
    startProductSubscription_domain,
    startProductSubscription_identityProvider,
    startProductSubscription_product,
    startProductSubscription_username,
    startProductSubscriptionResponse_httpStatus,
    startProductSubscriptionResponse_productUserSummary,

    -- ** StopProductSubscription
    stopProductSubscription_domain,
    stopProductSubscription_identityProvider,
    stopProductSubscription_product,
    stopProductSubscription_username,
    stopProductSubscriptionResponse_httpStatus,
    stopProductSubscriptionResponse_productUserSummary,

    -- ** UpdateIdentityProviderSettings
    updateIdentityProviderSettings_identityProvider,
    updateIdentityProviderSettings_product,
    updateIdentityProviderSettings_updateSettings,
    updateIdentityProviderSettingsResponse_httpStatus,
    updateIdentityProviderSettingsResponse_identityProviderSummary,

    -- * Types

    -- ** ActiveDirectoryIdentityProvider
    activeDirectoryIdentityProvider_directoryId,

    -- ** Filter
    filter_attribute,
    filter_operation,
    filter_value,

    -- ** IdentityProvider
    identityProvider_activeDirectoryIdentityProvider,

    -- ** IdentityProviderSummary
    identityProviderSummary_failureMessage,
    identityProviderSummary_identityProvider,
    identityProviderSummary_product,
    identityProviderSummary_settings,
    identityProviderSummary_status,

    -- ** InstanceSummary
    instanceSummary_lastStatusCheckDate,
    instanceSummary_statusMessage,
    instanceSummary_instanceId,
    instanceSummary_products,
    instanceSummary_status,

    -- ** InstanceUserSummary
    instanceUserSummary_associationDate,
    instanceUserSummary_disassociationDate,
    instanceUserSummary_domain,
    instanceUserSummary_statusMessage,
    instanceUserSummary_identityProvider,
    instanceUserSummary_instanceId,
    instanceUserSummary_status,
    instanceUserSummary_username,

    -- ** ProductUserSummary
    productUserSummary_domain,
    productUserSummary_statusMessage,
    productUserSummary_subscriptionEndDate,
    productUserSummary_subscriptionStartDate,
    productUserSummary_identityProvider,
    productUserSummary_product,
    productUserSummary_status,
    productUserSummary_username,

    -- ** Settings
    settings_securityGroupId,
    settings_subnets,

    -- ** UpdateSettings
    updateSettings_securityGroupId,
    updateSettings_addSubnets,
    updateSettings_removeSubnets,
  )
where

import Amazonka.LicenseManagerUserSubscriptions.AssociateUser
import Amazonka.LicenseManagerUserSubscriptions.DeregisterIdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.DisassociateUser
import Amazonka.LicenseManagerUserSubscriptions.ListIdentityProviders
import Amazonka.LicenseManagerUserSubscriptions.ListInstances
import Amazonka.LicenseManagerUserSubscriptions.ListProductSubscriptions
import Amazonka.LicenseManagerUserSubscriptions.ListUserAssociations
import Amazonka.LicenseManagerUserSubscriptions.RegisterIdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.StartProductSubscription
import Amazonka.LicenseManagerUserSubscriptions.StopProductSubscription
import Amazonka.LicenseManagerUserSubscriptions.Types.ActiveDirectoryIdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.Types.Filter
import Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProvider
import Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProviderSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.InstanceSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.InstanceUserSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.ProductUserSummary
import Amazonka.LicenseManagerUserSubscriptions.Types.Settings
import Amazonka.LicenseManagerUserSubscriptions.Types.UpdateSettings
import Amazonka.LicenseManagerUserSubscriptions.UpdateIdentityProviderSettings
