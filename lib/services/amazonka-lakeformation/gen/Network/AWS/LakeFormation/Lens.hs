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

    -- ** BatchRevokePermissions
    batchRevokePermissions_catalogId,
    batchRevokePermissions_entries,
    batchRevokePermissionsResponse_failures,
    batchRevokePermissionsResponse_httpStatus,

    -- ** DescribeResource
    describeResource_resourceArn,
    describeResourceResponse_resourceInfo,
    describeResourceResponse_httpStatus,

    -- ** BatchGrantPermissions
    batchGrantPermissions_catalogId,
    batchGrantPermissions_entries,
    batchGrantPermissionsResponse_failures,
    batchGrantPermissionsResponse_httpStatus,

    -- ** GetEffectivePermissionsForPath
    getEffectivePermissionsForPath_catalogId,
    getEffectivePermissionsForPath_nextToken,
    getEffectivePermissionsForPath_maxResults,
    getEffectivePermissionsForPath_resourceArn,
    getEffectivePermissionsForPathResponse_nextToken,
    getEffectivePermissionsForPathResponse_permissions,
    getEffectivePermissionsForPathResponse_httpStatus,

    -- ** RevokePermissions
    revokePermissions_catalogId,
    revokePermissions_permissionsWithGrantOption,
    revokePermissions_principal,
    revokePermissions_resource,
    revokePermissions_permissions,
    revokePermissionsResponse_httpStatus,

    -- ** UpdateResource
    updateResource_roleArn,
    updateResource_resourceArn,
    updateResourceResponse_httpStatus,

    -- ** AddLFTagsToResource
    addLFTagsToResource_catalogId,
    addLFTagsToResource_resource,
    addLFTagsToResource_lFTags,
    addLFTagsToResourceResponse_failures,
    addLFTagsToResourceResponse_httpStatus,

    -- ** SearchTablesByLFTags
    searchTablesByLFTags_catalogId,
    searchTablesByLFTags_nextToken,
    searchTablesByLFTags_maxResults,
    searchTablesByLFTags_expression,
    searchTablesByLFTagsResponse_tableList,
    searchTablesByLFTagsResponse_nextToken,
    searchTablesByLFTagsResponse_httpStatus,

    -- ** ListResources
    listResources_filterConditionList,
    listResources_nextToken,
    listResources_maxResults,
    listResourcesResponse_resourceInfoList,
    listResourcesResponse_nextToken,
    listResourcesResponse_httpStatus,

    -- ** GetLFTag
    getLFTag_catalogId,
    getLFTag_tagKey,
    getLFTagResponse_tagValues,
    getLFTagResponse_catalogId,
    getLFTagResponse_tagKey,
    getLFTagResponse_httpStatus,

    -- ** RemoveLFTagsFromResource
    removeLFTagsFromResource_catalogId,
    removeLFTagsFromResource_resource,
    removeLFTagsFromResource_lFTags,
    removeLFTagsFromResourceResponse_failures,
    removeLFTagsFromResourceResponse_httpStatus,

    -- ** UpdateLFTag
    updateLFTag_catalogId,
    updateLFTag_tagValuesToAdd,
    updateLFTag_tagValuesToDelete,
    updateLFTag_tagKey,
    updateLFTagResponse_httpStatus,

    -- ** DeleteLFTag
    deleteLFTag_catalogId,
    deleteLFTag_tagKey,
    deleteLFTagResponse_httpStatus,

    -- ** CreateLFTag
    createLFTag_catalogId,
    createLFTag_tagKey,
    createLFTag_tagValues,
    createLFTagResponse_httpStatus,

    -- ** GetResourceLFTags
    getResourceLFTags_showAssignedLFTags,
    getResourceLFTags_catalogId,
    getResourceLFTags_resource,
    getResourceLFTagsResponse_lFTagsOnTable,
    getResourceLFTagsResponse_lFTagOnDatabase,
    getResourceLFTagsResponse_lFTagsOnColumns,
    getResourceLFTagsResponse_httpStatus,

    -- ** PutDataLakeSettings
    putDataLakeSettings_catalogId,
    putDataLakeSettings_dataLakeSettings,
    putDataLakeSettingsResponse_httpStatus,

    -- ** ListPermissions
    listPermissions_resourceType,
    listPermissions_catalogId,
    listPermissions_nextToken,
    listPermissions_principal,
    listPermissions_resource,
    listPermissions_maxResults,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_principalResourcePermissions,
    listPermissionsResponse_httpStatus,

    -- ** DeregisterResource
    deregisterResource_resourceArn,
    deregisterResourceResponse_httpStatus,

    -- ** GetDataLakeSettings
    getDataLakeSettings_catalogId,
    getDataLakeSettingsResponse_dataLakeSettings,
    getDataLakeSettingsResponse_httpStatus,

    -- ** SearchDatabasesByLFTags
    searchDatabasesByLFTags_catalogId,
    searchDatabasesByLFTags_nextToken,
    searchDatabasesByLFTags_maxResults,
    searchDatabasesByLFTags_expression,
    searchDatabasesByLFTagsResponse_databaseList,
    searchDatabasesByLFTagsResponse_nextToken,
    searchDatabasesByLFTagsResponse_httpStatus,

    -- ** RegisterResource
    registerResource_useServiceLinkedRole,
    registerResource_roleArn,
    registerResource_resourceArn,
    registerResourceResponse_httpStatus,

    -- ** GrantPermissions
    grantPermissions_catalogId,
    grantPermissions_permissionsWithGrantOption,
    grantPermissions_principal,
    grantPermissions_resource,
    grantPermissions_permissions,
    grantPermissionsResponse_httpStatus,

    -- ** ListLFTags
    listLFTags_resourceShareType,
    listLFTags_catalogId,
    listLFTags_nextToken,
    listLFTags_maxResults,
    listLFTagsResponse_nextToken,
    listLFTagsResponse_lFTags,
    listLFTagsResponse_httpStatus,

    -- * Types

    -- ** BatchPermissionsFailureEntry
    batchPermissionsFailureEntry_error,
    batchPermissionsFailureEntry_requestEntry,

    -- ** BatchPermissionsRequestEntry
    batchPermissionsRequestEntry_permissionsWithGrantOption,
    batchPermissionsRequestEntry_principal,
    batchPermissionsRequestEntry_resource,
    batchPermissionsRequestEntry_permissions,
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
    dataLakeSettings_trustedResourceOwners,
    dataLakeSettings_createDatabaseDefaultPermissions,
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
    errorDetail_errorCode,
    errorDetail_errorMessage,

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
    principalResourcePermissions_additionalDetails,
    principalResourcePermissions_permissionsWithGrantOption,
    principalResourcePermissions_principal,
    principalResourcePermissions_resource,
    principalResourcePermissions_permissions,

    -- ** Resource
    resource_dataLocation,
    resource_database,
    resource_lFTag,
    resource_catalog,
    resource_lFTagPolicy,
    resource_table,
    resource_tableWithColumns,

    -- ** ResourceInfo
    resourceInfo_resourceArn,
    resourceInfo_lastModified,
    resourceInfo_roleArn,

    -- ** TableResource
    tableResource_catalogId,
    tableResource_tableWildcard,
    tableResource_name,
    tableResource_databaseName,

    -- ** TableWildcard

    -- ** TableWithColumnsResource
    tableWithColumnsResource_catalogId,
    tableWithColumnsResource_columnWildcard,
    tableWithColumnsResource_columnNames,
    tableWithColumnsResource_databaseName,
    tableWithColumnsResource_name,

    -- ** TaggedDatabase
    taggedDatabase_database,
    taggedDatabase_lFTags,

    -- ** TaggedTable
    taggedTable_lFTagsOnTable,
    taggedTable_lFTagOnDatabase,
    taggedTable_lFTagsOnColumns,
    taggedTable_table,
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
