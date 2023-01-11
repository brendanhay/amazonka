{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMSAP.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    getApplicationResponse_application,
    getApplicationResponse_tags,
    getApplicationResponse_httpStatus,

    -- ** GetComponent
    getComponent_applicationId,
    getComponent_componentId,
    getComponentResponse_component,
    getComponentResponse_httpStatus,

    -- ** GetDatabase
    getDatabase_applicationId,
    getDatabase_componentId,
    getDatabase_databaseArn,
    getDatabase_databaseId,
    getDatabaseResponse_database,
    getDatabaseResponse_tags,
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
    listApplications_maxResults,
    listApplications_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListComponents
    listComponents_applicationId,
    listComponents_maxResults,
    listComponents_nextToken,
    listComponentsResponse_components,
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,

    -- ** ListDatabases
    listDatabases_applicationId,
    listDatabases_componentId,
    listDatabases_maxResults,
    listDatabases_nextToken,
    listDatabasesResponse_databases,
    listDatabasesResponse_nextToken,
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
    registerApplication_sapInstanceNumber,
    registerApplication_sid,
    registerApplication_tags,
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
    updateApplicationSettings_credentialsToAddOrUpdate,
    updateApplicationSettings_credentialsToRemove,
    updateApplicationSettings_applicationId,
    updateApplicationSettingsResponse_message,
    updateApplicationSettingsResponse_operationIds,
    updateApplicationSettingsResponse_httpStatus,

    -- * Types

    -- ** Application
    application_appRegistryArn,
    application_arn,
    application_components,
    application_id,
    application_lastUpdated,
    application_status,
    application_statusMessage,
    application_type,

    -- ** ApplicationCredential
    applicationCredential_databaseName,
    applicationCredential_credentialType,
    applicationCredential_secretId,

    -- ** ApplicationSummary
    applicationSummary_arn,
    applicationSummary_id,
    applicationSummary_tags,
    applicationSummary_type,

    -- ** Component
    component_applicationId,
    component_componentId,
    component_componentType,
    component_databases,
    component_hosts,
    component_lastUpdated,
    component_primaryHost,
    component_status,

    -- ** ComponentSummary
    componentSummary_applicationId,
    componentSummary_componentId,
    componentSummary_componentType,
    componentSummary_tags,

    -- ** Database
    database_applicationId,
    database_arn,
    database_componentId,
    database_credentials,
    database_databaseId,
    database_databaseName,
    database_databaseType,
    database_lastUpdated,
    database_primaryHost,
    database_sQLPort,
    database_status,

    -- ** DatabaseSummary
    databaseSummary_applicationId,
    databaseSummary_arn,
    databaseSummary_componentId,
    databaseSummary_databaseId,
    databaseSummary_databaseType,
    databaseSummary_tags,

    -- ** Host
    host_hostIp,
    host_hostName,
    host_hostRole,
    host_instanceId,

    -- ** Operation
    operation_endTime,
    operation_id,
    operation_lastUpdatedTime,
    operation_properties,
    operation_resourceArn,
    operation_resourceId,
    operation_resourceType,
    operation_startTime,
    operation_status,
    operation_statusMessage,
    operation_type,
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
