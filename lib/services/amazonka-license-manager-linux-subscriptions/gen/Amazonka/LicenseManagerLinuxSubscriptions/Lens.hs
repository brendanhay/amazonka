{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerLinuxSubscriptions.Lens
  ( -- * Operations

    -- ** GetServiceSettings
    getServiceSettingsResponse_homeRegions,
    getServiceSettingsResponse_linuxSubscriptionsDiscovery,
    getServiceSettingsResponse_linuxSubscriptionsDiscoverySettings,
    getServiceSettingsResponse_status,
    getServiceSettingsResponse_statusMessage,
    getServiceSettingsResponse_httpStatus,

    -- ** ListLinuxSubscriptionInstances
    listLinuxSubscriptionInstances_filters,
    listLinuxSubscriptionInstances_maxResults,
    listLinuxSubscriptionInstances_nextToken,
    listLinuxSubscriptionInstancesResponse_instances,
    listLinuxSubscriptionInstancesResponse_nextToken,
    listLinuxSubscriptionInstancesResponse_httpStatus,

    -- ** ListLinuxSubscriptions
    listLinuxSubscriptions_filters,
    listLinuxSubscriptions_maxResults,
    listLinuxSubscriptions_nextToken,
    listLinuxSubscriptionsResponse_nextToken,
    listLinuxSubscriptionsResponse_subscriptions,
    listLinuxSubscriptionsResponse_httpStatus,

    -- ** UpdateServiceSettings
    updateServiceSettings_allowUpdate,
    updateServiceSettings_linuxSubscriptionsDiscovery,
    updateServiceSettings_linuxSubscriptionsDiscoverySettings,
    updateServiceSettingsResponse_homeRegions,
    updateServiceSettingsResponse_linuxSubscriptionsDiscovery,
    updateServiceSettingsResponse_linuxSubscriptionsDiscoverySettings,
    updateServiceSettingsResponse_status,
    updateServiceSettingsResponse_statusMessage,
    updateServiceSettingsResponse_httpStatus,

    -- * Types

    -- ** Filter
    filter_name,
    filter_operator,
    filter_values,

    -- ** Instance
    instance_accountID,
    instance_amiId,
    instance_instanceID,
    instance_instanceType,
    instance_lastUpdatedTime,
    instance_productCode,
    instance_region,
    instance_status,
    instance_subscriptionName,
    instance_usageOperation,

    -- ** LinuxSubscriptionsDiscoverySettings
    linuxSubscriptionsDiscoverySettings_organizationIntegration,
    linuxSubscriptionsDiscoverySettings_sourceRegions,

    -- ** Subscription
    subscription_instanceCount,
    subscription_name,
    subscription_type,
  )
where

import Amazonka.LicenseManagerLinuxSubscriptions.GetServiceSettings
import Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptionInstances
import Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptions
import Amazonka.LicenseManagerLinuxSubscriptions.Types.Filter
import Amazonka.LicenseManagerLinuxSubscriptions.Types.Instance
import Amazonka.LicenseManagerLinuxSubscriptions.Types.LinuxSubscriptionsDiscoverySettings
import Amazonka.LicenseManagerLinuxSubscriptions.Types.Subscription
import Amazonka.LicenseManagerLinuxSubscriptions.UpdateServiceSettings
