{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Schemas
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EventBridge Schema Registry
module Network.AWS.Schemas
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** PreconditionFailedException
    _PreconditionFailedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** GoneException
    _GoneException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- ** CodeBindingExists
    newCodeBindingExists,

    -- * Operations
    -- $operations

    -- ** UpdateRegistry
    UpdateRegistry (UpdateRegistry'),
    newUpdateRegistry,
    UpdateRegistryResponse (UpdateRegistryResponse'),
    newUpdateRegistryResponse,

    -- ** DeleteRegistry
    DeleteRegistry (DeleteRegistry'),
    newDeleteRegistry,
    DeleteRegistryResponse (DeleteRegistryResponse'),
    newDeleteRegistryResponse,

    -- ** SearchSchemas (Paginated)
    SearchSchemas (SearchSchemas'),
    newSearchSchemas,
    SearchSchemasResponse (SearchSchemasResponse'),
    newSearchSchemasResponse,

    -- ** StopDiscoverer
    StopDiscoverer (StopDiscoverer'),
    newStopDiscoverer,
    StopDiscovererResponse (StopDiscovererResponse'),
    newStopDiscovererResponse,

    -- ** DeleteSchemaVersion
    DeleteSchemaVersion (DeleteSchemaVersion'),
    newDeleteSchemaVersion,
    DeleteSchemaVersionResponse (DeleteSchemaVersionResponse'),
    newDeleteSchemaVersionResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListSchemaVersions (Paginated)
    ListSchemaVersions (ListSchemaVersions'),
    newListSchemaVersions,
    ListSchemaVersionsResponse (ListSchemaVersionsResponse'),
    newListSchemaVersionsResponse,

    -- ** ExportSchema
    ExportSchema (ExportSchema'),
    newExportSchema,
    ExportSchemaResponse (ExportSchemaResponse'),
    newExportSchemaResponse,

    -- ** GetDiscoveredSchema
    GetDiscoveredSchema (GetDiscoveredSchema'),
    newGetDiscoveredSchema,
    GetDiscoveredSchemaResponse (GetDiscoveredSchemaResponse'),
    newGetDiscoveredSchemaResponse,

    -- ** DeleteDiscoverer
    DeleteDiscoverer (DeleteDiscoverer'),
    newDeleteDiscoverer,
    DeleteDiscovererResponse (DeleteDiscovererResponse'),
    newDeleteDiscovererResponse,

    -- ** UpdateDiscoverer
    UpdateDiscoverer (UpdateDiscoverer'),
    newUpdateDiscoverer,
    UpdateDiscovererResponse (UpdateDiscovererResponse'),
    newUpdateDiscovererResponse,

    -- ** ListDiscoverers (Paginated)
    ListDiscoverers (ListDiscoverers'),
    newListDiscoverers,
    ListDiscoverersResponse (ListDiscoverersResponse'),
    newListDiscoverersResponse,

    -- ** ListSchemas (Paginated)
    ListSchemas (ListSchemas'),
    newListSchemas,
    ListSchemasResponse (ListSchemasResponse'),
    newListSchemasResponse,

    -- ** CreateDiscoverer
    CreateDiscoverer (CreateDiscoverer'),
    newCreateDiscoverer,
    CreateDiscovererResponse (CreateDiscovererResponse'),
    newCreateDiscovererResponse,

    -- ** DescribeRegistry
    DescribeRegistry (DescribeRegistry'),
    newDescribeRegistry,
    DescribeRegistryResponse (DescribeRegistryResponse'),
    newDescribeRegistryResponse,

    -- ** CreateRegistry
    CreateRegistry (CreateRegistry'),
    newCreateRegistry,
    CreateRegistryResponse (CreateRegistryResponse'),
    newCreateRegistryResponse,

    -- ** ListRegistries (Paginated)
    ListRegistries (ListRegistries'),
    newListRegistries,
    ListRegistriesResponse (ListRegistriesResponse'),
    newListRegistriesResponse,

    -- ** DescribeDiscoverer
    DescribeDiscoverer (DescribeDiscoverer'),
    newDescribeDiscoverer,
    DescribeDiscovererResponse (DescribeDiscovererResponse'),
    newDescribeDiscovererResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** StartDiscoverer
    StartDiscoverer (StartDiscoverer'),
    newStartDiscoverer,
    StartDiscovererResponse (StartDiscovererResponse'),
    newStartDiscovererResponse,

    -- ** DescribeSchema
    DescribeSchema (DescribeSchema'),
    newDescribeSchema,
    DescribeSchemaResponse (DescribeSchemaResponse'),
    newDescribeSchemaResponse,

    -- ** GetCodeBindingSource
    GetCodeBindingSource (GetCodeBindingSource'),
    newGetCodeBindingSource,
    GetCodeBindingSourceResponse (GetCodeBindingSourceResponse'),
    newGetCodeBindingSourceResponse,

    -- ** PutCodeBinding
    PutCodeBinding (PutCodeBinding'),
    newPutCodeBinding,
    PutCodeBindingResponse (PutCodeBindingResponse'),
    newPutCodeBindingResponse,

    -- ** CreateSchema
    CreateSchema (CreateSchema'),
    newCreateSchema,
    CreateSchemaResponse (CreateSchemaResponse'),
    newCreateSchemaResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UpdateSchema
    UpdateSchema (UpdateSchema'),
    newUpdateSchema,
    UpdateSchemaResponse (UpdateSchemaResponse'),
    newUpdateSchemaResponse,

    -- ** DeleteSchema
    DeleteSchema (DeleteSchema'),
    newDeleteSchema,
    DeleteSchemaResponse (DeleteSchemaResponse'),
    newDeleteSchemaResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeCodeBinding
    DescribeCodeBinding (DescribeCodeBinding'),
    newDescribeCodeBinding,
    DescribeCodeBindingResponse (DescribeCodeBindingResponse'),
    newDescribeCodeBindingResponse,

    -- * Types

    -- ** CodeGenerationStatus
    CodeGenerationStatus (..),

    -- ** DiscovererState
    DiscovererState (..),

    -- ** Type
    Type (..),

    -- ** DiscovererSummary
    DiscovererSummary (DiscovererSummary'),
    newDiscovererSummary,

    -- ** RegistrySummary
    RegistrySummary (RegistrySummary'),
    newRegistrySummary,

    -- ** SchemaSummary
    SchemaSummary (SchemaSummary'),
    newSchemaSummary,

    -- ** SchemaVersionSummary
    SchemaVersionSummary (SchemaVersionSummary'),
    newSchemaVersionSummary,

    -- ** SearchSchemaSummary
    SearchSchemaSummary (SearchSchemaSummary'),
    newSearchSchemaSummary,

    -- ** SearchSchemaVersionSummary
    SearchSchemaVersionSummary (SearchSchemaVersionSummary'),
    newSearchSchemaVersionSummary,
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
import Network.AWS.Schemas.Lens
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
import Network.AWS.Schemas.Types
import Network.AWS.Schemas.UntagResource
import Network.AWS.Schemas.UpdateDiscoverer
import Network.AWS.Schemas.UpdateRegistry
import Network.AWS.Schemas.UpdateSchema
import Network.AWS.Schemas.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Schemas'.

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
