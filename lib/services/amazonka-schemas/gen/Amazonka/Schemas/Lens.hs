{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Schemas.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Lens
  ( -- * Operations

    -- ** CreateDiscoverer
    createDiscoverer_crossAccount,
    createDiscoverer_description,
    createDiscoverer_tags,
    createDiscoverer_sourceArn,
    createDiscovererResponse_crossAccount,
    createDiscovererResponse_description,
    createDiscovererResponse_discovererArn,
    createDiscovererResponse_discovererId,
    createDiscovererResponse_sourceArn,
    createDiscovererResponse_state,
    createDiscovererResponse_tags,
    createDiscovererResponse_httpStatus,

    -- ** CreateRegistry
    createRegistry_description,
    createRegistry_tags,
    createRegistry_registryName,
    createRegistryResponse_description,
    createRegistryResponse_registryArn,
    createRegistryResponse_registryName,
    createRegistryResponse_tags,
    createRegistryResponse_httpStatus,

    -- ** CreateSchema
    createSchema_description,
    createSchema_tags,
    createSchema_registryName,
    createSchema_schemaName,
    createSchema_type,
    createSchema_content,
    createSchemaResponse_description,
    createSchemaResponse_lastModified,
    createSchemaResponse_schemaArn,
    createSchemaResponse_schemaName,
    createSchemaResponse_schemaVersion,
    createSchemaResponse_tags,
    createSchemaResponse_type,
    createSchemaResponse_versionCreatedDate,
    createSchemaResponse_httpStatus,

    -- ** DeleteDiscoverer
    deleteDiscoverer_discovererId,

    -- ** DeleteRegistry
    deleteRegistry_registryName,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_registryName,

    -- ** DeleteSchema
    deleteSchema_registryName,
    deleteSchema_schemaName,

    -- ** DeleteSchemaVersion
    deleteSchemaVersion_schemaVersion,
    deleteSchemaVersion_registryName,
    deleteSchemaVersion_schemaName,

    -- ** DescribeCodeBinding
    describeCodeBinding_schemaVersion,
    describeCodeBinding_registryName,
    describeCodeBinding_schemaName,
    describeCodeBinding_language,
    describeCodeBindingResponse_creationDate,
    describeCodeBindingResponse_lastModified,
    describeCodeBindingResponse_schemaVersion,
    describeCodeBindingResponse_status,
    describeCodeBindingResponse_httpStatus,

    -- ** DescribeDiscoverer
    describeDiscoverer_discovererId,
    describeDiscovererResponse_crossAccount,
    describeDiscovererResponse_description,
    describeDiscovererResponse_discovererArn,
    describeDiscovererResponse_discovererId,
    describeDiscovererResponse_sourceArn,
    describeDiscovererResponse_state,
    describeDiscovererResponse_tags,
    describeDiscovererResponse_httpStatus,

    -- ** DescribeRegistry
    describeRegistry_registryName,
    describeRegistryResponse_description,
    describeRegistryResponse_registryArn,
    describeRegistryResponse_registryName,
    describeRegistryResponse_tags,
    describeRegistryResponse_httpStatus,

    -- ** DescribeSchema
    describeSchema_schemaVersion,
    describeSchema_registryName,
    describeSchema_schemaName,
    describeSchemaResponse_content,
    describeSchemaResponse_description,
    describeSchemaResponse_lastModified,
    describeSchemaResponse_schemaArn,
    describeSchemaResponse_schemaName,
    describeSchemaResponse_schemaVersion,
    describeSchemaResponse_tags,
    describeSchemaResponse_type,
    describeSchemaResponse_versionCreatedDate,
    describeSchemaResponse_httpStatus,

    -- ** ExportSchema
    exportSchema_schemaVersion,
    exportSchema_registryName,
    exportSchema_schemaName,
    exportSchema_type,
    exportSchemaResponse_content,
    exportSchemaResponse_schemaArn,
    exportSchemaResponse_schemaName,
    exportSchemaResponse_schemaVersion,
    exportSchemaResponse_type,
    exportSchemaResponse_httpStatus,

    -- ** GetCodeBindingSource
    getCodeBindingSource_schemaVersion,
    getCodeBindingSource_registryName,
    getCodeBindingSource_schemaName,
    getCodeBindingSource_language,
    getCodeBindingSourceResponse_body,
    getCodeBindingSourceResponse_httpStatus,

    -- ** GetDiscoveredSchema
    getDiscoveredSchema_type,
    getDiscoveredSchema_events,
    getDiscoveredSchemaResponse_content,
    getDiscoveredSchemaResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_registryName,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_revisionId,
    getResourcePolicyResponse_httpStatus,

    -- ** ListDiscoverers
    listDiscoverers_discovererIdPrefix,
    listDiscoverers_limit,
    listDiscoverers_nextToken,
    listDiscoverers_sourceArnPrefix,
    listDiscoverersResponse_discoverers,
    listDiscoverersResponse_nextToken,
    listDiscoverersResponse_httpStatus,

    -- ** ListRegistries
    listRegistries_limit,
    listRegistries_nextToken,
    listRegistries_registryNamePrefix,
    listRegistries_scope,
    listRegistriesResponse_nextToken,
    listRegistriesResponse_registries,
    listRegistriesResponse_httpStatus,

    -- ** ListSchemaVersions
    listSchemaVersions_limit,
    listSchemaVersions_nextToken,
    listSchemaVersions_registryName,
    listSchemaVersions_schemaName,
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_schemaVersions,
    listSchemaVersionsResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_limit,
    listSchemas_nextToken,
    listSchemas_schemaNamePrefix,
    listSchemas_registryName,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutCodeBinding
    putCodeBinding_schemaVersion,
    putCodeBinding_registryName,
    putCodeBinding_schemaName,
    putCodeBinding_language,
    putCodeBindingResponse_creationDate,
    putCodeBindingResponse_lastModified,
    putCodeBindingResponse_schemaVersion,
    putCodeBindingResponse_status,
    putCodeBindingResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_registryName,
    putResourcePolicy_revisionId,
    putResourcePolicy_policy,
    putResourcePolicyResponse_policy,
    putResourcePolicyResponse_revisionId,
    putResourcePolicyResponse_httpStatus,

    -- ** SearchSchemas
    searchSchemas_limit,
    searchSchemas_nextToken,
    searchSchemas_registryName,
    searchSchemas_keywords,
    searchSchemasResponse_nextToken,
    searchSchemasResponse_schemas,
    searchSchemasResponse_httpStatus,

    -- ** StartDiscoverer
    startDiscoverer_discovererId,
    startDiscovererResponse_discovererId,
    startDiscovererResponse_state,
    startDiscovererResponse_httpStatus,

    -- ** StopDiscoverer
    stopDiscoverer_discovererId,
    stopDiscovererResponse_discovererId,
    stopDiscovererResponse_state,
    stopDiscovererResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdateDiscoverer
    updateDiscoverer_crossAccount,
    updateDiscoverer_description,
    updateDiscoverer_discovererId,
    updateDiscovererResponse_crossAccount,
    updateDiscovererResponse_description,
    updateDiscovererResponse_discovererArn,
    updateDiscovererResponse_discovererId,
    updateDiscovererResponse_sourceArn,
    updateDiscovererResponse_state,
    updateDiscovererResponse_tags,
    updateDiscovererResponse_httpStatus,

    -- ** UpdateRegistry
    updateRegistry_description,
    updateRegistry_registryName,
    updateRegistryResponse_description,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_registryName,
    updateRegistryResponse_tags,
    updateRegistryResponse_httpStatus,

    -- ** UpdateSchema
    updateSchema_clientTokenId,
    updateSchema_content,
    updateSchema_description,
    updateSchema_type,
    updateSchema_registryName,
    updateSchema_schemaName,
    updateSchemaResponse_description,
    updateSchemaResponse_lastModified,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_schemaVersion,
    updateSchemaResponse_tags,
    updateSchemaResponse_type,
    updateSchemaResponse_versionCreatedDate,
    updateSchemaResponse_httpStatus,

    -- * Types

    -- ** DiscovererSummary
    discovererSummary_crossAccount,
    discovererSummary_discovererArn,
    discovererSummary_discovererId,
    discovererSummary_sourceArn,
    discovererSummary_state,
    discovererSummary_tags,

    -- ** RegistrySummary
    registrySummary_registryArn,
    registrySummary_registryName,
    registrySummary_tags,

    -- ** SchemaSummary
    schemaSummary_lastModified,
    schemaSummary_schemaArn,
    schemaSummary_schemaName,
    schemaSummary_tags,
    schemaSummary_versionCount,

    -- ** SchemaVersionSummary
    schemaVersionSummary_schemaArn,
    schemaVersionSummary_schemaName,
    schemaVersionSummary_schemaVersion,
    schemaVersionSummary_type,

    -- ** SearchSchemaSummary
    searchSchemaSummary_registryName,
    searchSchemaSummary_schemaArn,
    searchSchemaSummary_schemaName,
    searchSchemaSummary_schemaVersions,

    -- ** SearchSchemaVersionSummary
    searchSchemaVersionSummary_createdDate,
    searchSchemaVersionSummary_schemaVersion,
    searchSchemaVersionSummary_type,
  )
where

import Amazonka.Schemas.CreateDiscoverer
import Amazonka.Schemas.CreateRegistry
import Amazonka.Schemas.CreateSchema
import Amazonka.Schemas.DeleteDiscoverer
import Amazonka.Schemas.DeleteRegistry
import Amazonka.Schemas.DeleteResourcePolicy
import Amazonka.Schemas.DeleteSchema
import Amazonka.Schemas.DeleteSchemaVersion
import Amazonka.Schemas.DescribeCodeBinding
import Amazonka.Schemas.DescribeDiscoverer
import Amazonka.Schemas.DescribeRegistry
import Amazonka.Schemas.DescribeSchema
import Amazonka.Schemas.ExportSchema
import Amazonka.Schemas.GetCodeBindingSource
import Amazonka.Schemas.GetDiscoveredSchema
import Amazonka.Schemas.GetResourcePolicy
import Amazonka.Schemas.ListDiscoverers
import Amazonka.Schemas.ListRegistries
import Amazonka.Schemas.ListSchemaVersions
import Amazonka.Schemas.ListSchemas
import Amazonka.Schemas.ListTagsForResource
import Amazonka.Schemas.PutCodeBinding
import Amazonka.Schemas.PutResourcePolicy
import Amazonka.Schemas.SearchSchemas
import Amazonka.Schemas.StartDiscoverer
import Amazonka.Schemas.StopDiscoverer
import Amazonka.Schemas.TagResource
import Amazonka.Schemas.Types.DiscovererSummary
import Amazonka.Schemas.Types.RegistrySummary
import Amazonka.Schemas.Types.SchemaSummary
import Amazonka.Schemas.Types.SchemaVersionSummary
import Amazonka.Schemas.Types.SearchSchemaSummary
import Amazonka.Schemas.Types.SearchSchemaVersionSummary
import Amazonka.Schemas.UntagResource
import Amazonka.Schemas.UpdateDiscoverer
import Amazonka.Schemas.UpdateRegistry
import Amazonka.Schemas.UpdateSchema
