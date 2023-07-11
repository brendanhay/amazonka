{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SSMSAP
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This API reference provides descriptions, syntax, and other details
-- about each of the actions and data types for AWS Systems Manager for
-- SAP. The topic for each action shows the API request parameters and
-- responses.
module Amazonka.SSMSAP
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteResourcePermission
    DeleteResourcePermission (DeleteResourcePermission'),
    newDeleteResourcePermission,
    DeleteResourcePermissionResponse (DeleteResourcePermissionResponse'),
    newDeleteResourcePermissionResponse,

    -- ** DeregisterApplication
    DeregisterApplication (DeregisterApplication'),
    newDeregisterApplication,
    DeregisterApplicationResponse (DeregisterApplicationResponse'),
    newDeregisterApplicationResponse,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    GetApplicationResponse (GetApplicationResponse'),
    newGetApplicationResponse,

    -- ** GetComponent
    GetComponent (GetComponent'),
    newGetComponent,
    GetComponentResponse (GetComponentResponse'),
    newGetComponentResponse,

    -- ** GetDatabase
    GetDatabase (GetDatabase'),
    newGetDatabase,
    GetDatabaseResponse (GetDatabaseResponse'),
    newGetDatabaseResponse,

    -- ** GetOperation
    GetOperation (GetOperation'),
    newGetOperation,
    GetOperationResponse (GetOperationResponse'),
    newGetOperationResponse,

    -- ** GetResourcePermission
    GetResourcePermission (GetResourcePermission'),
    newGetResourcePermission,
    GetResourcePermissionResponse (GetResourcePermissionResponse'),
    newGetResourcePermissionResponse,

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** ListComponents (Paginated)
    ListComponents (ListComponents'),
    newListComponents,
    ListComponentsResponse (ListComponentsResponse'),
    newListComponentsResponse,

    -- ** ListDatabases (Paginated)
    ListDatabases (ListDatabases'),
    newListDatabases,
    ListDatabasesResponse (ListDatabasesResponse'),
    newListDatabasesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutResourcePermission
    PutResourcePermission (PutResourcePermission'),
    newPutResourcePermission,
    PutResourcePermissionResponse (PutResourcePermissionResponse'),
    newPutResourcePermissionResponse,

    -- ** RegisterApplication
    RegisterApplication (RegisterApplication'),
    newRegisterApplication,
    RegisterApplicationResponse (RegisterApplicationResponse'),
    newRegisterApplicationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApplicationSettings
    UpdateApplicationSettings (UpdateApplicationSettings'),
    newUpdateApplicationSettings,
    UpdateApplicationSettingsResponse (UpdateApplicationSettingsResponse'),
    newUpdateApplicationSettingsResponse,

    -- * Types

    -- ** ApplicationStatus
    ApplicationStatus (..),

    -- ** ApplicationType
    ApplicationType (..),

    -- ** ComponentStatus
    ComponentStatus (..),

    -- ** ComponentType
    ComponentType (..),

    -- ** CredentialType
    CredentialType (..),

    -- ** DatabaseStatus
    DatabaseStatus (..),

    -- ** DatabaseType
    DatabaseType (..),

    -- ** HostRole
    HostRole (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** PermissionActionType
    PermissionActionType (..),

    -- ** Application
    Application (Application'),
    newApplication,

    -- ** ApplicationCredential
    ApplicationCredential (ApplicationCredential'),
    newApplicationCredential,

    -- ** ApplicationSummary
    ApplicationSummary (ApplicationSummary'),
    newApplicationSummary,

    -- ** Component
    Component (Component'),
    newComponent,

    -- ** ComponentSummary
    ComponentSummary (ComponentSummary'),
    newComponentSummary,

    -- ** Database
    Database (Database'),
    newDatabase,

    -- ** DatabaseSummary
    DatabaseSummary (DatabaseSummary'),
    newDatabaseSummary,

    -- ** Host
    Host (Host'),
    newHost,

    -- ** Operation
    Operation (Operation'),
    newOperation,
  )
where

import Amazonka.SSMSAP.DeleteResourcePermission
import Amazonka.SSMSAP.DeregisterApplication
import Amazonka.SSMSAP.GetApplication
import Amazonka.SSMSAP.GetComponent
import Amazonka.SSMSAP.GetDatabase
import Amazonka.SSMSAP.GetOperation
import Amazonka.SSMSAP.GetResourcePermission
import Amazonka.SSMSAP.Lens
import Amazonka.SSMSAP.ListApplications
import Amazonka.SSMSAP.ListComponents
import Amazonka.SSMSAP.ListDatabases
import Amazonka.SSMSAP.ListTagsForResource
import Amazonka.SSMSAP.PutResourcePermission
import Amazonka.SSMSAP.RegisterApplication
import Amazonka.SSMSAP.TagResource
import Amazonka.SSMSAP.Types
import Amazonka.SSMSAP.UntagResource
import Amazonka.SSMSAP.UpdateApplicationSettings
import Amazonka.SSMSAP.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SSMSAP'.

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
