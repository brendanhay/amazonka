{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMSAP.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Lens
  ( -- * Operations

    -- ** DeleteResourcePermission
    deleteResourcePermission_actionType,
    deleteResourcePermission_sourceResourceArn,
    deleteResourcePermission_resourceArn,
    deleteResourcePermissionResponse_policy,
    deleteResourcePermissionResponse_httpStatus,

    -- ** DeregisterApplication
    deregisterApplication_applicationId,
    deregisterApplicationResponse_httpStatus,

    -- ** GetApplication
    getApplication_applicationArn,
    getApplication_applicationId,
    getApplicationResponse_tags,
    getApplicationResponse_application,
    getApplicationResponse_httpStatus,

    -- ** GetComponent
    getComponent_applicationId,
    getComponent_componentId,
    getComponentResponse_component,
    getComponentResponse_httpStatus,

    -- ** GetDatabase
    getDatabase_databaseId,
    getDatabase_componentId,
    getDatabase_applicationId,
    getDatabase_databaseArn,
    getDatabaseResponse_tags,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,

    -- ** GetOperation
    getOperation_operationId,
    getOperationResponse_operation,
    getOperationResponse_httpStatus,

    -- ** GetResourcePermission
    getResourcePermission_actionType,
    getResourcePermission_resourceArn,
    getResourcePermissionResponse_policy,
    getResourcePermissionResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,

    -- ** ListComponents
    listComponents_nextToken,
    listComponents_maxResults,
    listComponents_applicationId,
    listComponentsResponse_nextToken,
    listComponentsResponse_components,
    listComponentsResponse_httpStatus,

    -- ** ListDatabases
    listDatabases_nextToken,
    listDatabases_maxResults,
    listDatabases_componentId,
    listDatabases_applicationId,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_databases,
    listDatabasesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutResourcePermission
    putResourcePermission_actionType,
    putResourcePermission_sourceResourceArn,
    putResourcePermission_resourceArn,
    putResourcePermissionResponse_policy,
    putResourcePermissionResponse_httpStatus,

    -- ** RegisterApplication
    registerApplication_tags,
    registerApplication_sid,
    registerApplication_sapInstanceNumber,
    registerApplication_applicationId,
    registerApplication_applicationType,
    registerApplication_instances,
    registerApplication_credentials,
    registerApplicationResponse_application,
    registerApplicationResponse_operationId,
    registerApplicationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplicationSettings
    updateApplicationSettings_credentialsToRemove,
    updateApplicationSettings_credentialsToAddOrUpdate,
    updateApplicationSettings_applicationId,
    updateApplicationSettingsResponse_message,
    updateApplicationSettingsResponse_operationIds,
    updateApplicationSettingsResponse_httpStatus,

    -- * Types

    -- ** Application
    application_type,
    application_appRegistryArn,
    application_arn,
    application_status,
    application_id,
    application_components,
    application_lastUpdated,
    application_statusMessage,

    -- ** ApplicationCredential
    applicationCredential_databaseName,
    applicationCredential_credentialType,
    applicationCredential_secretId,

    -- ** ApplicationSummary
    applicationSummary_tags,
    applicationSummary_type,
    applicationSummary_arn,
    applicationSummary_id,

    -- ** Component
    component_primaryHost,
    component_status,
    component_hosts,
    component_lastUpdated,
    component_componentId,
    component_applicationId,
    component_componentType,
    component_databases,

    -- ** ComponentSummary
    componentSummary_tags,
    componentSummary_componentId,
    componentSummary_applicationId,
    componentSummary_componentType,

    -- ** Database
    database_primaryHost,
    database_sQLPort,
    database_databaseName,
    database_arn,
    database_databaseType,
    database_databaseId,
    database_status,
    database_lastUpdated,
    database_credentials,
    database_componentId,
    database_applicationId,

    -- ** DatabaseSummary
    databaseSummary_tags,
    databaseSummary_arn,
    databaseSummary_databaseType,
    databaseSummary_databaseId,
    databaseSummary_componentId,
    databaseSummary_applicationId,

    -- ** Host
    host_hostRole,
    host_hostIp,
    host_hostName,
    host_instanceId,

    -- ** Operation
    operation_resourceId,
    operation_resourceType,
    operation_type,
    operation_properties,
    operation_status,
    operation_lastUpdatedTime,
    operation_endTime,
    operation_id,
    operation_resourceArn,
    operation_statusMessage,
    operation_startTime,
  )
where

import Amazonka.SSMSAP.DeleteResourcePermission
import Amazonka.SSMSAP.DeregisterApplication
import Amazonka.SSMSAP.GetApplication
import Amazonka.SSMSAP.GetComponent
import Amazonka.SSMSAP.GetDatabase
import Amazonka.SSMSAP.GetOperation
import Amazonka.SSMSAP.GetResourcePermission
import Amazonka.SSMSAP.ListApplications
import Amazonka.SSMSAP.ListComponents
import Amazonka.SSMSAP.ListDatabases
import Amazonka.SSMSAP.ListTagsForResource
import Amazonka.SSMSAP.PutResourcePermission
import Amazonka.SSMSAP.RegisterApplication
import Amazonka.SSMSAP.TagResource
import Amazonka.SSMSAP.Types.Application
import Amazonka.SSMSAP.Types.ApplicationCredential
import Amazonka.SSMSAP.Types.ApplicationSummary
import Amazonka.SSMSAP.Types.Component
import Amazonka.SSMSAP.Types.ComponentSummary
import Amazonka.SSMSAP.Types.Database
import Amazonka.SSMSAP.Types.DatabaseSummary
import Amazonka.SSMSAP.Types.Host
import Amazonka.SSMSAP.Types.Operation
import Amazonka.SSMSAP.UntagResource
import Amazonka.SSMSAP.UpdateApplicationSettings
