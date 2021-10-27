{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Schemas.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Schemas.Lens
  ( -- * Operations

    -- ** UpdateRegistry
    updateRegistry_description,
    updateRegistry_registryName,
    updateRegistryResponse_registryName,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_description,
    updateRegistryResponse_tags,
    updateRegistryResponse_httpStatus,

    -- ** DeleteRegistry
    deleteRegistry_registryName,

    -- ** SearchSchemas
    searchSchemas_nextToken,
    searchSchemas_limit,
    searchSchemas_registryName,
    searchSchemas_keywords,
    searchSchemasResponse_schemas,
    searchSchemasResponse_nextToken,
    searchSchemasResponse_httpStatus,

    -- ** StopDiscoverer
    stopDiscoverer_discovererId,
    stopDiscovererResponse_state,
    stopDiscovererResponse_discovererId,
    stopDiscovererResponse_httpStatus,

    -- ** DeleteSchemaVersion
    deleteSchemaVersion_schemaVersion,
    deleteSchemaVersion_registryName,
    deleteSchemaVersion_schemaName,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListSchemaVersions
    listSchemaVersions_nextToken,
    listSchemaVersions_limit,
    listSchemaVersions_registryName,
    listSchemaVersions_schemaName,
    listSchemaVersionsResponse_schemaVersions,
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_httpStatus,

    -- ** ExportSchema
    exportSchema_schemaVersion,
    exportSchema_registryName,
    exportSchema_schemaName,
    exportSchema_type,
    exportSchemaResponse_schemaVersion,
    exportSchemaResponse_schemaName,
    exportSchemaResponse_content,
    exportSchemaResponse_schemaArn,
    exportSchemaResponse_type,
    exportSchemaResponse_httpStatus,

    -- ** GetDiscoveredSchema
    getDiscoveredSchema_type,
    getDiscoveredSchema_events,
    getDiscoveredSchemaResponse_content,
    getDiscoveredSchemaResponse_httpStatus,

    -- ** DeleteDiscoverer
    deleteDiscoverer_discovererId,

    -- ** UpdateDiscoverer
    updateDiscoverer_crossAccount,
    updateDiscoverer_description,
    updateDiscoverer_discovererId,
    updateDiscovererResponse_state,
    updateDiscovererResponse_crossAccount,
    updateDiscovererResponse_sourceArn,
    updateDiscovererResponse_discovererId,
    updateDiscovererResponse_description,
    updateDiscovererResponse_tags,
    updateDiscovererResponse_discovererArn,
    updateDiscovererResponse_httpStatus,

    -- ** ListDiscoverers
    listDiscoverers_sourceArnPrefix,
    listDiscoverers_discovererIdPrefix,
    listDiscoverers_nextToken,
    listDiscoverers_limit,
    listDiscoverersResponse_discoverers,
    listDiscoverersResponse_nextToken,
    listDiscoverersResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_schemaNamePrefix,
    listSchemas_nextToken,
    listSchemas_limit,
    listSchemas_registryName,
    listSchemasResponse_schemas,
    listSchemasResponse_nextToken,
    listSchemasResponse_httpStatus,

    -- ** CreateDiscoverer
    createDiscoverer_crossAccount,
    createDiscoverer_description,
    createDiscoverer_tags,
    createDiscoverer_sourceArn,
    createDiscovererResponse_state,
    createDiscovererResponse_crossAccount,
    createDiscovererResponse_sourceArn,
    createDiscovererResponse_discovererId,
    createDiscovererResponse_description,
    createDiscovererResponse_tags,
    createDiscovererResponse_discovererArn,
    createDiscovererResponse_httpStatus,

    -- ** DescribeRegistry
    describeRegistry_registryName,
    describeRegistryResponse_registryName,
    describeRegistryResponse_registryArn,
    describeRegistryResponse_description,
    describeRegistryResponse_tags,
    describeRegistryResponse_httpStatus,

    -- ** CreateRegistry
    createRegistry_description,
    createRegistry_tags,
    createRegistry_registryName,
    createRegistryResponse_registryName,
    createRegistryResponse_registryArn,
    createRegistryResponse_description,
    createRegistryResponse_tags,
    createRegistryResponse_httpStatus,

    -- ** ListRegistries
    listRegistries_registryNamePrefix,
    listRegistries_nextToken,
    listRegistries_scope,
    listRegistries_limit,
    listRegistriesResponse_registries,
    listRegistriesResponse_nextToken,
    listRegistriesResponse_httpStatus,

    -- ** DescribeDiscoverer
    describeDiscoverer_discovererId,
    describeDiscovererResponse_state,
    describeDiscovererResponse_crossAccount,
    describeDiscovererResponse_sourceArn,
    describeDiscovererResponse_discovererId,
    describeDiscovererResponse_description,
    describeDiscovererResponse_tags,
    describeDiscovererResponse_discovererArn,
    describeDiscovererResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_registryName,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_revisionId,
    getResourcePolicyResponse_httpStatus,

    -- ** StartDiscoverer
    startDiscoverer_discovererId,
    startDiscovererResponse_state,
    startDiscovererResponse_discovererId,
    startDiscovererResponse_httpStatus,

    -- ** DescribeSchema
    describeSchema_schemaVersion,
    describeSchema_registryName,
    describeSchema_schemaName,
    describeSchemaResponse_schemaVersion,
    describeSchemaResponse_schemaName,
    describeSchemaResponse_content,
    describeSchemaResponse_schemaArn,
    describeSchemaResponse_type,
    describeSchemaResponse_lastModified,
    describeSchemaResponse_description,
    describeSchemaResponse_versionCreatedDate,
    describeSchemaResponse_tags,
    describeSchemaResponse_httpStatus,

    -- ** GetCodeBindingSource
    getCodeBindingSource_schemaVersion,
    getCodeBindingSource_registryName,
    getCodeBindingSource_schemaName,
    getCodeBindingSource_language,
    getCodeBindingSourceResponse_body,
    getCodeBindingSourceResponse_httpStatus,

    -- ** PutCodeBinding
    putCodeBinding_schemaVersion,
    putCodeBinding_registryName,
    putCodeBinding_schemaName,
    putCodeBinding_language,
    putCodeBindingResponse_status,
    putCodeBindingResponse_schemaVersion,
    putCodeBindingResponse_creationDate,
    putCodeBindingResponse_lastModified,
    putCodeBindingResponse_httpStatus,

    -- ** CreateSchema
    createSchema_description,
    createSchema_tags,
    createSchema_registryName,
    createSchema_schemaName,
    createSchema_type,
    createSchema_content,
    createSchemaResponse_schemaVersion,
    createSchemaResponse_schemaName,
    createSchemaResponse_schemaArn,
    createSchemaResponse_type,
    createSchemaResponse_lastModified,
    createSchemaResponse_description,
    createSchemaResponse_versionCreatedDate,
    createSchemaResponse_tags,
    createSchemaResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UpdateSchema
    updateSchema_content,
    updateSchema_clientTokenId,
    updateSchema_type,
    updateSchema_description,
    updateSchema_registryName,
    updateSchema_schemaName,
    updateSchemaResponse_schemaVersion,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_type,
    updateSchemaResponse_lastModified,
    updateSchemaResponse_description,
    updateSchemaResponse_versionCreatedDate,
    updateSchemaResponse_tags,
    updateSchemaResponse_httpStatus,

    -- ** DeleteSchema
    deleteSchema_registryName,
    deleteSchema_schemaName,

    -- ** PutResourcePolicy
    putResourcePolicy_registryName,
    putResourcePolicy_revisionId,
    putResourcePolicy_policy,
    putResourcePolicyResponse_policy,
    putResourcePolicyResponse_revisionId,
    putResourcePolicyResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_registryName,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** DescribeCodeBinding
    describeCodeBinding_schemaVersion,
    describeCodeBinding_registryName,
    describeCodeBinding_schemaName,
    describeCodeBinding_language,
    describeCodeBindingResponse_status,
    describeCodeBindingResponse_schemaVersion,
    describeCodeBindingResponse_creationDate,
    describeCodeBindingResponse_lastModified,
    describeCodeBindingResponse_httpStatus,

    -- * Types

    -- ** DiscovererSummary
    discovererSummary_state,
    discovererSummary_crossAccount,
    discovererSummary_sourceArn,
    discovererSummary_discovererId,
    discovererSummary_tags,
    discovererSummary_discovererArn,

    -- ** RegistrySummary
    registrySummary_registryName,
    registrySummary_registryArn,
    registrySummary_tags,

    -- ** SchemaSummary
    schemaSummary_schemaName,
    schemaSummary_schemaArn,
    schemaSummary_lastModified,
    schemaSummary_tags,
    schemaSummary_versionCount,

    -- ** SchemaVersionSummary
    schemaVersionSummary_schemaVersion,
    schemaVersionSummary_schemaName,
    schemaVersionSummary_schemaArn,
    schemaVersionSummary_type,

    -- ** SearchSchemaSummary
    searchSchemaSummary_registryName,
    searchSchemaSummary_schemaVersions,
    searchSchemaSummary_schemaName,
    searchSchemaSummary_schemaArn,

    -- ** SearchSchemaVersionSummary
    searchSchemaVersionSummary_schemaVersion,
    searchSchemaVersionSummary_createdDate,
    searchSchemaVersionSummary_type,
  )
where

import Network.AWS.Schemas.CreateDiscoverer
import Network.AWS.Schemas.CreateRegistry
import Network.AWS.Schemas.CreateSchema
import Network.AWS.Schemas.DeleteDiscoverer
import Network.AWS.Schemas.DeleteRegistry
import Network.AWS.Schemas.DeleteResourcePolicy
import Network.AWS.Schemas.DeleteSchema
import Network.AWS.Schemas.DeleteSchemaVersion
import Network.AWS.Schemas.DescribeCodeBinding
import Network.AWS.Schemas.DescribeDiscoverer
import Network.AWS.Schemas.DescribeRegistry
import Network.AWS.Schemas.DescribeSchema
import Network.AWS.Schemas.ExportSchema
import Network.AWS.Schemas.GetCodeBindingSource
import Network.AWS.Schemas.GetDiscoveredSchema
import Network.AWS.Schemas.GetResourcePolicy
import Network.AWS.Schemas.ListDiscoverers
import Network.AWS.Schemas.ListRegistries
import Network.AWS.Schemas.ListSchemaVersions
import Network.AWS.Schemas.ListSchemas
import Network.AWS.Schemas.ListTagsForResource
import Network.AWS.Schemas.PutCodeBinding
import Network.AWS.Schemas.PutResourcePolicy
import Network.AWS.Schemas.SearchSchemas
import Network.AWS.Schemas.StartDiscoverer
import Network.AWS.Schemas.StopDiscoverer
import Network.AWS.Schemas.TagResource
import Network.AWS.Schemas.Types.DiscovererSummary
import Network.AWS.Schemas.Types.RegistrySummary
import Network.AWS.Schemas.Types.SchemaSummary
import Network.AWS.Schemas.Types.SchemaVersionSummary
import Network.AWS.Schemas.Types.SearchSchemaSummary
import Network.AWS.Schemas.Types.SearchSchemaVersionSummary
import Network.AWS.Schemas.UntagResource
import Network.AWS.Schemas.UpdateDiscoverer
import Network.AWS.Schemas.UpdateRegistry
import Network.AWS.Schemas.UpdateSchema
