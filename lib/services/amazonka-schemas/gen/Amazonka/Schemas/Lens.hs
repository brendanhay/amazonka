{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Schemas.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Lens
  ( -- * Operations

    -- ** CreateDiscoverer
    createDiscoverer_tags,
    createDiscoverer_description,
    createDiscoverer_crossAccount,
    createDiscoverer_sourceArn,
    createDiscovererResponse_tags,
    createDiscovererResponse_discovererId,
    createDiscovererResponse_sourceArn,
    createDiscovererResponse_state,
    createDiscovererResponse_description,
    createDiscovererResponse_crossAccount,
    createDiscovererResponse_discovererArn,
    createDiscovererResponse_httpStatus,

    -- ** CreateRegistry
    createRegistry_tags,
    createRegistry_description,
    createRegistry_registryName,
    createRegistryResponse_tags,
    createRegistryResponse_registryName,
    createRegistryResponse_description,
    createRegistryResponse_registryArn,
    createRegistryResponse_httpStatus,

    -- ** CreateSchema
    createSchema_tags,
    createSchema_description,
    createSchema_registryName,
    createSchema_schemaName,
    createSchema_type,
    createSchema_content,
    createSchemaResponse_tags,
    createSchemaResponse_type,
    createSchemaResponse_schemaName,
    createSchemaResponse_description,
    createSchemaResponse_schemaArn,
    createSchemaResponse_lastModified,
    createSchemaResponse_schemaVersion,
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
    describeCodeBindingResponse_status,
    describeCodeBindingResponse_lastModified,
    describeCodeBindingResponse_schemaVersion,
    describeCodeBindingResponse_httpStatus,

    -- ** DescribeDiscoverer
    describeDiscoverer_discovererId,
    describeDiscovererResponse_tags,
    describeDiscovererResponse_discovererId,
    describeDiscovererResponse_sourceArn,
    describeDiscovererResponse_state,
    describeDiscovererResponse_description,
    describeDiscovererResponse_crossAccount,
    describeDiscovererResponse_discovererArn,
    describeDiscovererResponse_httpStatus,

    -- ** DescribeRegistry
    describeRegistry_registryName,
    describeRegistryResponse_tags,
    describeRegistryResponse_registryName,
    describeRegistryResponse_description,
    describeRegistryResponse_registryArn,
    describeRegistryResponse_httpStatus,

    -- ** DescribeSchema
    describeSchema_schemaVersion,
    describeSchema_registryName,
    describeSchema_schemaName,
    describeSchemaResponse_tags,
    describeSchemaResponse_type,
    describeSchemaResponse_schemaName,
    describeSchemaResponse_description,
    describeSchemaResponse_schemaArn,
    describeSchemaResponse_lastModified,
    describeSchemaResponse_schemaVersion,
    describeSchemaResponse_content,
    describeSchemaResponse_versionCreatedDate,
    describeSchemaResponse_httpStatus,

    -- ** ExportSchema
    exportSchema_schemaVersion,
    exportSchema_registryName,
    exportSchema_schemaName,
    exportSchema_type,
    exportSchemaResponse_type,
    exportSchemaResponse_schemaName,
    exportSchemaResponse_schemaArn,
    exportSchemaResponse_schemaVersion,
    exportSchemaResponse_content,
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
    listDiscoverers_nextToken,
    listDiscoverers_discovererIdPrefix,
    listDiscoverers_sourceArnPrefix,
    listDiscoverers_limit,
    listDiscoverersResponse_nextToken,
    listDiscoverersResponse_discoverers,
    listDiscoverersResponse_httpStatus,

    -- ** ListRegistries
    listRegistries_nextToken,
    listRegistries_registryNamePrefix,
    listRegistries_limit,
    listRegistries_scope,
    listRegistriesResponse_nextToken,
    listRegistriesResponse_registries,
    listRegistriesResponse_httpStatus,

    -- ** ListSchemaVersions
    listSchemaVersions_nextToken,
    listSchemaVersions_limit,
    listSchemaVersions_registryName,
    listSchemaVersions_schemaName,
    listSchemaVersionsResponse_schemaVersions,
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_nextToken,
    listSchemas_schemaNamePrefix,
    listSchemas_limit,
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
    putCodeBindingResponse_status,
    putCodeBindingResponse_lastModified,
    putCodeBindingResponse_schemaVersion,
    putCodeBindingResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_registryName,
    putResourcePolicy_revisionId,
    putResourcePolicy_policy,
    putResourcePolicyResponse_policy,
    putResourcePolicyResponse_revisionId,
    putResourcePolicyResponse_httpStatus,

    -- ** SearchSchemas
    searchSchemas_nextToken,
    searchSchemas_limit,
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
    updateDiscoverer_description,
    updateDiscoverer_crossAccount,
    updateDiscoverer_discovererId,
    updateDiscovererResponse_tags,
    updateDiscovererResponse_discovererId,
    updateDiscovererResponse_sourceArn,
    updateDiscovererResponse_state,
    updateDiscovererResponse_description,
    updateDiscovererResponse_crossAccount,
    updateDiscovererResponse_discovererArn,
    updateDiscovererResponse_httpStatus,

    -- ** UpdateRegistry
    updateRegistry_description,
    updateRegistry_registryName,
    updateRegistryResponse_tags,
    updateRegistryResponse_registryName,
    updateRegistryResponse_description,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_httpStatus,

    -- ** UpdateSchema
    updateSchema_type,
    updateSchema_clientTokenId,
    updateSchema_description,
    updateSchema_content,
    updateSchema_registryName,
    updateSchema_schemaName,
    updateSchemaResponse_tags,
    updateSchemaResponse_type,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_description,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_lastModified,
    updateSchemaResponse_schemaVersion,
    updateSchemaResponse_versionCreatedDate,
    updateSchemaResponse_httpStatus,

    -- * Types

    -- ** DiscovererSummary
    discovererSummary_tags,
    discovererSummary_discovererId,
    discovererSummary_sourceArn,
    discovererSummary_state,
    discovererSummary_crossAccount,
    discovererSummary_discovererArn,

    -- ** RegistrySummary
    registrySummary_tags,
    registrySummary_registryName,
    registrySummary_registryArn,

    -- ** SchemaSummary
    schemaSummary_tags,
    schemaSummary_schemaName,
    schemaSummary_schemaArn,
    schemaSummary_lastModified,
    schemaSummary_versionCount,

    -- ** SchemaVersionSummary
    schemaVersionSummary_type,
    schemaVersionSummary_schemaName,
    schemaVersionSummary_schemaArn,
    schemaVersionSummary_schemaVersion,

    -- ** SearchSchemaSummary
    searchSchemaSummary_registryName,
    searchSchemaSummary_schemaVersions,
    searchSchemaSummary_schemaName,
    searchSchemaSummary_schemaArn,

    -- ** SearchSchemaVersionSummary
    searchSchemaVersionSummary_type,
    searchSchemaVersionSummary_schemaVersion,
    searchSchemaVersionSummary_createdDate,
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
