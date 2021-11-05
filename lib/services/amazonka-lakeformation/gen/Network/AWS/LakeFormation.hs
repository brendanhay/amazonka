{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LakeFormation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Lake Formation
--
-- Defines the public endpoint for the AWS Lake Formation service.
module Amazonka.LakeFormation
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** OperationTimeoutException
    _OperationTimeoutException,

    -- ** EntityNotFoundException
    _EntityNotFoundException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ResourceNumberLimitExceededException
    _ResourceNumberLimitExceededException,

    -- ** GlueEncryptionException
    _GlueEncryptionException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchRevokePermissions
    BatchRevokePermissions (BatchRevokePermissions'),
    newBatchRevokePermissions,
    BatchRevokePermissionsResponse (BatchRevokePermissionsResponse'),
    newBatchRevokePermissionsResponse,

    -- ** DescribeResource
    DescribeResource (DescribeResource'),
    newDescribeResource,
    DescribeResourceResponse (DescribeResourceResponse'),
    newDescribeResourceResponse,

    -- ** BatchGrantPermissions
    BatchGrantPermissions (BatchGrantPermissions'),
    newBatchGrantPermissions,
    BatchGrantPermissionsResponse (BatchGrantPermissionsResponse'),
    newBatchGrantPermissionsResponse,

    -- ** GetEffectivePermissionsForPath
    GetEffectivePermissionsForPath (GetEffectivePermissionsForPath'),
    newGetEffectivePermissionsForPath,
    GetEffectivePermissionsForPathResponse (GetEffectivePermissionsForPathResponse'),
    newGetEffectivePermissionsForPathResponse,

    -- ** RevokePermissions
    RevokePermissions (RevokePermissions'),
    newRevokePermissions,
    RevokePermissionsResponse (RevokePermissionsResponse'),
    newRevokePermissionsResponse,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    UpdateResourceResponse (UpdateResourceResponse'),
    newUpdateResourceResponse,

    -- ** AddLFTagsToResource
    AddLFTagsToResource (AddLFTagsToResource'),
    newAddLFTagsToResource,
    AddLFTagsToResourceResponse (AddLFTagsToResourceResponse'),
    newAddLFTagsToResourceResponse,

    -- ** SearchTablesByLFTags
    SearchTablesByLFTags (SearchTablesByLFTags'),
    newSearchTablesByLFTags,
    SearchTablesByLFTagsResponse (SearchTablesByLFTagsResponse'),
    newSearchTablesByLFTagsResponse,

    -- ** ListResources
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** GetLFTag
    GetLFTag (GetLFTag'),
    newGetLFTag,
    GetLFTagResponse (GetLFTagResponse'),
    newGetLFTagResponse,

    -- ** RemoveLFTagsFromResource
    RemoveLFTagsFromResource (RemoveLFTagsFromResource'),
    newRemoveLFTagsFromResource,
    RemoveLFTagsFromResourceResponse (RemoveLFTagsFromResourceResponse'),
    newRemoveLFTagsFromResourceResponse,

    -- ** UpdateLFTag
    UpdateLFTag (UpdateLFTag'),
    newUpdateLFTag,
    UpdateLFTagResponse (UpdateLFTagResponse'),
    newUpdateLFTagResponse,

    -- ** DeleteLFTag
    DeleteLFTag (DeleteLFTag'),
    newDeleteLFTag,
    DeleteLFTagResponse (DeleteLFTagResponse'),
    newDeleteLFTagResponse,

    -- ** CreateLFTag
    CreateLFTag (CreateLFTag'),
    newCreateLFTag,
    CreateLFTagResponse (CreateLFTagResponse'),
    newCreateLFTagResponse,

    -- ** GetResourceLFTags
    GetResourceLFTags (GetResourceLFTags'),
    newGetResourceLFTags,
    GetResourceLFTagsResponse (GetResourceLFTagsResponse'),
    newGetResourceLFTagsResponse,

    -- ** PutDataLakeSettings
    PutDataLakeSettings (PutDataLakeSettings'),
    newPutDataLakeSettings,
    PutDataLakeSettingsResponse (PutDataLakeSettingsResponse'),
    newPutDataLakeSettingsResponse,

    -- ** ListPermissions
    ListPermissions (ListPermissions'),
    newListPermissions,
    ListPermissionsResponse (ListPermissionsResponse'),
    newListPermissionsResponse,

    -- ** DeregisterResource
    DeregisterResource (DeregisterResource'),
    newDeregisterResource,
    DeregisterResourceResponse (DeregisterResourceResponse'),
    newDeregisterResourceResponse,

    -- ** GetDataLakeSettings
    GetDataLakeSettings (GetDataLakeSettings'),
    newGetDataLakeSettings,
    GetDataLakeSettingsResponse (GetDataLakeSettingsResponse'),
    newGetDataLakeSettingsResponse,

    -- ** SearchDatabasesByLFTags
    SearchDatabasesByLFTags (SearchDatabasesByLFTags'),
    newSearchDatabasesByLFTags,
    SearchDatabasesByLFTagsResponse (SearchDatabasesByLFTagsResponse'),
    newSearchDatabasesByLFTagsResponse,

    -- ** RegisterResource
    RegisterResource (RegisterResource'),
    newRegisterResource,
    RegisterResourceResponse (RegisterResourceResponse'),
    newRegisterResourceResponse,

    -- ** GrantPermissions
    GrantPermissions (GrantPermissions'),
    newGrantPermissions,
    GrantPermissionsResponse (GrantPermissionsResponse'),
    newGrantPermissionsResponse,

    -- ** ListLFTags
    ListLFTags (ListLFTags'),
    newListLFTags,
    ListLFTagsResponse (ListLFTagsResponse'),
    newListLFTagsResponse,

    -- * Types

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** DataLakeResourceType
    DataLakeResourceType (..),

    -- ** FieldNameString
    FieldNameString (..),

    -- ** Permission
    Permission (..),

    -- ** ResourceShareType
    ResourceShareType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** BatchPermissionsFailureEntry
    BatchPermissionsFailureEntry (BatchPermissionsFailureEntry'),
    newBatchPermissionsFailureEntry,

    -- ** BatchPermissionsRequestEntry
    BatchPermissionsRequestEntry (BatchPermissionsRequestEntry'),
    newBatchPermissionsRequestEntry,

    -- ** CatalogResource
    CatalogResource (CatalogResource'),
    newCatalogResource,

    -- ** ColumnLFTag
    ColumnLFTag (ColumnLFTag'),
    newColumnLFTag,

    -- ** ColumnWildcard
    ColumnWildcard (ColumnWildcard'),
    newColumnWildcard,

    -- ** DataLakePrincipal
    DataLakePrincipal (DataLakePrincipal'),
    newDataLakePrincipal,

    -- ** DataLakeSettings
    DataLakeSettings (DataLakeSettings'),
    newDataLakeSettings,

    -- ** DataLocationResource
    DataLocationResource (DataLocationResource'),
    newDataLocationResource,

    -- ** DatabaseResource
    DatabaseResource (DatabaseResource'),
    newDatabaseResource,

    -- ** DetailsMap
    DetailsMap (DetailsMap'),
    newDetailsMap,

    -- ** ErrorDetail
    ErrorDetail (ErrorDetail'),
    newErrorDetail,

    -- ** FilterCondition
    FilterCondition (FilterCondition'),
    newFilterCondition,

    -- ** LFTag
    LFTag (LFTag'),
    newLFTag,

    -- ** LFTagError
    LFTagError (LFTagError'),
    newLFTagError,

    -- ** LFTagKeyResource
    LFTagKeyResource (LFTagKeyResource'),
    newLFTagKeyResource,

    -- ** LFTagPair
    LFTagPair (LFTagPair'),
    newLFTagPair,

    -- ** LFTagPolicyResource
    LFTagPolicyResource (LFTagPolicyResource'),
    newLFTagPolicyResource,

    -- ** PrincipalPermissions
    PrincipalPermissions (PrincipalPermissions'),
    newPrincipalPermissions,

    -- ** PrincipalResourcePermissions
    PrincipalResourcePermissions (PrincipalResourcePermissions'),
    newPrincipalResourcePermissions,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceInfo
    ResourceInfo (ResourceInfo'),
    newResourceInfo,

    -- ** TableResource
    TableResource (TableResource'),
    newTableResource,

    -- ** TableWildcard
    TableWildcard (TableWildcard'),
    newTableWildcard,

    -- ** TableWithColumnsResource
    TableWithColumnsResource (TableWithColumnsResource'),
    newTableWithColumnsResource,

    -- ** TaggedDatabase
    TaggedDatabase (TaggedDatabase'),
    newTaggedDatabase,

    -- ** TaggedTable
    TaggedTable (TaggedTable'),
    newTaggedTable,
  )
where

import Amazonka.LakeFormation.AddLFTagsToResource
import Amazonka.LakeFormation.BatchGrantPermissions
import Amazonka.LakeFormation.BatchRevokePermissions
import Amazonka.LakeFormation.CreateLFTag
import Amazonka.LakeFormation.DeleteLFTag
import Amazonka.LakeFormation.DeregisterResource
import Amazonka.LakeFormation.DescribeResource
import Amazonka.LakeFormation.GetDataLakeSettings
import Amazonka.LakeFormation.GetEffectivePermissionsForPath
import Amazonka.LakeFormation.GetLFTag
import Amazonka.LakeFormation.GetResourceLFTags
import Amazonka.LakeFormation.GrantPermissions
import Amazonka.LakeFormation.Lens
import Amazonka.LakeFormation.ListLFTags
import Amazonka.LakeFormation.ListPermissions
import Amazonka.LakeFormation.ListResources
import Amazonka.LakeFormation.PutDataLakeSettings
import Amazonka.LakeFormation.RegisterResource
import Amazonka.LakeFormation.RemoveLFTagsFromResource
import Amazonka.LakeFormation.RevokePermissions
import Amazonka.LakeFormation.SearchDatabasesByLFTags
import Amazonka.LakeFormation.SearchTablesByLFTags
import Amazonka.LakeFormation.Types
import Amazonka.LakeFormation.UpdateLFTag
import Amazonka.LakeFormation.UpdateResource
import Amazonka.LakeFormation.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LakeFormation'.

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
