{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LakeFormation.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Lens
  ( -- * Operations

    -- ** AddLFTagsToResource
    addLFTagsToResource_catalogId,
    addLFTagsToResource_resource,
    addLFTagsToResource_lFTags,
    addLFTagsToResourceResponse_failures,
    addLFTagsToResourceResponse_httpStatus,

    -- ** BatchGrantPermissions
    batchGrantPermissions_catalogId,
    batchGrantPermissions_entries,
    batchGrantPermissionsResponse_failures,
    batchGrantPermissionsResponse_httpStatus,

    -- ** BatchRevokePermissions
    batchRevokePermissions_catalogId,
    batchRevokePermissions_entries,
    batchRevokePermissionsResponse_failures,
    batchRevokePermissionsResponse_httpStatus,

    -- ** CreateLFTag
    createLFTag_catalogId,
    createLFTag_tagKey,
    createLFTag_tagValues,
    createLFTagResponse_httpStatus,

    -- ** DeleteLFTag
    deleteLFTag_catalogId,
    deleteLFTag_tagKey,
    deleteLFTagResponse_httpStatus,

    -- ** DeregisterResource
    deregisterResource_resourceArn,
    deregisterResourceResponse_httpStatus,

    -- ** DescribeResource
    describeResource_resourceArn,
    describeResourceResponse_resourceInfo,
    describeResourceResponse_httpStatus,

    -- ** GetDataLakeSettings
    getDataLakeSettings_catalogId,
    getDataLakeSettingsResponse_dataLakeSettings,
    getDataLakeSettingsResponse_httpStatus,

    -- ** GetEffectivePermissionsForPath
    getEffectivePermissionsForPath_nextToken,
    getEffectivePermissionsForPath_maxResults,
    getEffectivePermissionsForPath_catalogId,
    getEffectivePermissionsForPath_resourceArn,
    getEffectivePermissionsForPathResponse_nextToken,
    getEffectivePermissionsForPathResponse_permissions,
    getEffectivePermissionsForPathResponse_httpStatus,

    -- ** GetLFTag
    getLFTag_catalogId,
    getLFTag_tagKey,
    getLFTagResponse_tagValues,
    getLFTagResponse_tagKey,
    getLFTagResponse_catalogId,
    getLFTagResponse_httpStatus,

    -- ** GetResourceLFTags
    getResourceLFTags_catalogId,
    getResourceLFTags_showAssignedLFTags,
    getResourceLFTags_resource,
    getResourceLFTagsResponse_lFTagsOnTable,
    getResourceLFTagsResponse_lFTagOnDatabase,
    getResourceLFTagsResponse_lFTagsOnColumns,
    getResourceLFTagsResponse_httpStatus,

    -- ** GrantPermissions
    grantPermissions_catalogId,
    grantPermissions_permissionsWithGrantOption,
    grantPermissions_principal,
    grantPermissions_resource,
    grantPermissions_permissions,
    grantPermissionsResponse_httpStatus,

    -- ** ListLFTags
    listLFTags_nextToken,
    listLFTags_resourceShareType,
    listLFTags_maxResults,
    listLFTags_catalogId,
    listLFTagsResponse_nextToken,
    listLFTagsResponse_lFTags,
    listLFTagsResponse_httpStatus,

    -- ** ListPermissions
    listPermissions_principal,
    listPermissions_resourceType,
    listPermissions_nextToken,
    listPermissions_maxResults,
    listPermissions_catalogId,
    listPermissions_resource,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_principalResourcePermissions,
    listPermissionsResponse_httpStatus,

    -- ** ListResources
    listResources_nextToken,
    listResources_filterConditionList,
    listResources_maxResults,
    listResourcesResponse_nextToken,
    listResourcesResponse_resourceInfoList,
    listResourcesResponse_httpStatus,

    -- ** PutDataLakeSettings
    putDataLakeSettings_catalogId,
    putDataLakeSettings_dataLakeSettings,
    putDataLakeSettingsResponse_httpStatus,

    -- ** RegisterResource
    registerResource_roleArn,
    registerResource_useServiceLinkedRole,
    registerResource_resourceArn,
    registerResourceResponse_httpStatus,

    -- ** RemoveLFTagsFromResource
    removeLFTagsFromResource_catalogId,
    removeLFTagsFromResource_resource,
    removeLFTagsFromResource_lFTags,
    removeLFTagsFromResourceResponse_failures,
    removeLFTagsFromResourceResponse_httpStatus,

    -- ** RevokePermissions
    revokePermissions_catalogId,
    revokePermissions_permissionsWithGrantOption,
    revokePermissions_principal,
    revokePermissions_resource,
    revokePermissions_permissions,
    revokePermissionsResponse_httpStatus,

    -- ** SearchDatabasesByLFTags
    searchDatabasesByLFTags_nextToken,
    searchDatabasesByLFTags_maxResults,
    searchDatabasesByLFTags_catalogId,
    searchDatabasesByLFTags_expression,
    searchDatabasesByLFTagsResponse_nextToken,
    searchDatabasesByLFTagsResponse_databaseList,
    searchDatabasesByLFTagsResponse_httpStatus,

    -- ** SearchTablesByLFTags
    searchTablesByLFTags_nextToken,
    searchTablesByLFTags_maxResults,
    searchTablesByLFTags_catalogId,
    searchTablesByLFTags_expression,
    searchTablesByLFTagsResponse_nextToken,
    searchTablesByLFTagsResponse_tableList,
    searchTablesByLFTagsResponse_httpStatus,

    -- ** UpdateLFTag
    updateLFTag_catalogId,
    updateLFTag_tagValuesToDelete,
    updateLFTag_tagValuesToAdd,
    updateLFTag_tagKey,
    updateLFTagResponse_httpStatus,

    -- ** UpdateResource
    updateResource_roleArn,
    updateResource_resourceArn,
    updateResourceResponse_httpStatus,

    -- * Types

    -- ** BatchPermissionsFailureEntry
    batchPermissionsFailureEntry_requestEntry,
    batchPermissionsFailureEntry_error,

    -- ** BatchPermissionsRequestEntry
    batchPermissionsRequestEntry_principal,
    batchPermissionsRequestEntry_permissions,
    batchPermissionsRequestEntry_permissionsWithGrantOption,
    batchPermissionsRequestEntry_resource,
    batchPermissionsRequestEntry_id,

    -- ** CatalogResource

    -- ** ColumnLFTag
    columnLFTag_name,
    columnLFTag_lFTags,

    -- ** ColumnWildcard
    columnWildcard_excludedColumnNames,

    -- ** DataLakePrincipal
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- ** DataLakeSettings
    dataLakeSettings_dataLakeAdmins,
    dataLakeSettings_createDatabaseDefaultPermissions,
    dataLakeSettings_trustedResourceOwners,
    dataLakeSettings_createTableDefaultPermissions,

    -- ** DataLocationResource
    dataLocationResource_catalogId,
    dataLocationResource_resourceArn,

    -- ** DatabaseResource
    databaseResource_catalogId,
    databaseResource_name,

    -- ** DetailsMap
    detailsMap_resourceShare,

    -- ** ErrorDetail
    errorDetail_errorMessage,
    errorDetail_errorCode,

    -- ** FilterCondition
    filterCondition_field,
    filterCondition_comparisonOperator,
    filterCondition_stringValueList,

    -- ** LFTag
    lFTag_tagKey,
    lFTag_tagValues,

    -- ** LFTagError
    lFTagError_lFTag,
    lFTagError_error,

    -- ** LFTagKeyResource
    lFTagKeyResource_catalogId,
    lFTagKeyResource_tagKey,
    lFTagKeyResource_tagValues,

    -- ** LFTagPair
    lFTagPair_catalogId,
    lFTagPair_tagKey,
    lFTagPair_tagValues,

    -- ** LFTagPolicyResource
    lFTagPolicyResource_catalogId,
    lFTagPolicyResource_resourceType,
    lFTagPolicyResource_expression,

    -- ** PrincipalPermissions
    principalPermissions_principal,
    principalPermissions_permissions,

    -- ** PrincipalResourcePermissions
    principalResourcePermissions_principal,
    principalResourcePermissions_additionalDetails,
    principalResourcePermissions_permissions,
    principalResourcePermissions_permissionsWithGrantOption,
    principalResourcePermissions_resource,

    -- ** Resource
    resource_tableWithColumns,
    resource_catalog,
    resource_lFTag,
    resource_lFTagPolicy,
    resource_database,
    resource_dataLocation,
    resource_table,

    -- ** ResourceInfo
    resourceInfo_roleArn,
    resourceInfo_lastModified,
    resourceInfo_resourceArn,

    -- ** TableResource
    tableResource_name,
    tableResource_tableWildcard,
    tableResource_catalogId,
    tableResource_databaseName,

    -- ** TableWildcard

    -- ** TableWithColumnsResource
    tableWithColumnsResource_columnNames,
    tableWithColumnsResource_columnWildcard,
    tableWithColumnsResource_catalogId,
    tableWithColumnsResource_databaseName,
    tableWithColumnsResource_name,

    -- ** TaggedDatabase
    taggedDatabase_lFTags,
    taggedDatabase_database,

    -- ** TaggedTable
    taggedTable_lFTagsOnTable,
    taggedTable_table,
    taggedTable_lFTagOnDatabase,
    taggedTable_lFTagsOnColumns,
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
import Amazonka.LakeFormation.ListLFTags
import Amazonka.LakeFormation.ListPermissions
import Amazonka.LakeFormation.ListResources
import Amazonka.LakeFormation.PutDataLakeSettings
import Amazonka.LakeFormation.RegisterResource
import Amazonka.LakeFormation.RemoveLFTagsFromResource
import Amazonka.LakeFormation.RevokePermissions
import Amazonka.LakeFormation.SearchDatabasesByLFTags
import Amazonka.LakeFormation.SearchTablesByLFTags
import Amazonka.LakeFormation.Types.BatchPermissionsFailureEntry
import Amazonka.LakeFormation.Types.BatchPermissionsRequestEntry
import Amazonka.LakeFormation.Types.CatalogResource
import Amazonka.LakeFormation.Types.ColumnLFTag
import Amazonka.LakeFormation.Types.ColumnWildcard
import Amazonka.LakeFormation.Types.DataLakePrincipal
import Amazonka.LakeFormation.Types.DataLakeSettings
import Amazonka.LakeFormation.Types.DataLocationResource
import Amazonka.LakeFormation.Types.DatabaseResource
import Amazonka.LakeFormation.Types.DetailsMap
import Amazonka.LakeFormation.Types.ErrorDetail
import Amazonka.LakeFormation.Types.FilterCondition
import Amazonka.LakeFormation.Types.LFTag
import Amazonka.LakeFormation.Types.LFTagError
import Amazonka.LakeFormation.Types.LFTagKeyResource
import Amazonka.LakeFormation.Types.LFTagPair
import Amazonka.LakeFormation.Types.LFTagPolicyResource
import Amazonka.LakeFormation.Types.PrincipalPermissions
import Amazonka.LakeFormation.Types.PrincipalResourcePermissions
import Amazonka.LakeFormation.Types.Resource
import Amazonka.LakeFormation.Types.ResourceInfo
import Amazonka.LakeFormation.Types.TableResource
import Amazonka.LakeFormation.Types.TableWildcard
import Amazonka.LakeFormation.Types.TableWithColumnsResource
import Amazonka.LakeFormation.Types.TaggedDatabase
import Amazonka.LakeFormation.Types.TaggedTable
import Amazonka.LakeFormation.UpdateLFTag
import Amazonka.LakeFormation.UpdateResource
