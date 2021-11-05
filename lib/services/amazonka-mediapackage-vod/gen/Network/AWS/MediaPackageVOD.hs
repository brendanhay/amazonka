{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.MediaPackageVOD
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-07@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Elemental MediaPackage VOD
module Network.AWS.MediaPackageVOD
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreatePackagingGroup
    CreatePackagingGroup (CreatePackagingGroup'),
    newCreatePackagingGroup,
    CreatePackagingGroupResponse (CreatePackagingGroupResponse'),
    newCreatePackagingGroupResponse,

    -- ** ConfigureLogs
    ConfigureLogs (ConfigureLogs'),
    newConfigureLogs,
    ConfigureLogsResponse (ConfigureLogsResponse'),
    newConfigureLogsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribePackagingGroup
    DescribePackagingGroup (DescribePackagingGroup'),
    newDescribePackagingGroup,
    DescribePackagingGroupResponse (DescribePackagingGroupResponse'),
    newDescribePackagingGroupResponse,

    -- ** DescribeAsset
    DescribeAsset (DescribeAsset'),
    newDescribeAsset,
    DescribeAssetResponse (DescribeAssetResponse'),
    newDescribeAssetResponse,

    -- ** DeletePackagingConfiguration
    DeletePackagingConfiguration (DeletePackagingConfiguration'),
    newDeletePackagingConfiguration,
    DeletePackagingConfigurationResponse (DeletePackagingConfigurationResponse'),
    newDeletePackagingConfigurationResponse,

    -- ** ListPackagingGroups (Paginated)
    ListPackagingGroups (ListPackagingGroups'),
    newListPackagingGroups,
    ListPackagingGroupsResponse (ListPackagingGroupsResponse'),
    newListPackagingGroupsResponse,

    -- ** DeleteAsset
    DeleteAsset (DeleteAsset'),
    newDeleteAsset,
    DeleteAssetResponse (DeleteAssetResponse'),
    newDeleteAssetResponse,

    -- ** UpdatePackagingGroup
    UpdatePackagingGroup (UpdatePackagingGroup'),
    newUpdatePackagingGroup,
    UpdatePackagingGroupResponse (UpdatePackagingGroupResponse'),
    newUpdatePackagingGroupResponse,

    -- ** DeletePackagingGroup
    DeletePackagingGroup (DeletePackagingGroup'),
    newDeletePackagingGroup,
    DeletePackagingGroupResponse (DeletePackagingGroupResponse'),
    newDeletePackagingGroupResponse,

    -- ** CreateAsset
    CreateAsset (CreateAsset'),
    newCreateAsset,
    CreateAssetResponse (CreateAssetResponse'),
    newCreateAssetResponse,

    -- ** DescribePackagingConfiguration
    DescribePackagingConfiguration (DescribePackagingConfiguration'),
    newDescribePackagingConfiguration,
    DescribePackagingConfigurationResponse (DescribePackagingConfigurationResponse'),
    newDescribePackagingConfigurationResponse,

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

    -- ** CreatePackagingConfiguration
    CreatePackagingConfiguration (CreatePackagingConfiguration'),
    newCreatePackagingConfiguration,
    CreatePackagingConfigurationResponse (CreatePackagingConfigurationResponse'),
    newCreatePackagingConfigurationResponse,

    -- ** ListPackagingConfigurations (Paginated)
    ListPackagingConfigurations (ListPackagingConfigurations'),
    newListPackagingConfigurations,
    ListPackagingConfigurationsResponse (ListPackagingConfigurationsResponse'),
    newListPackagingConfigurationsResponse,

    -- ** ListAssets (Paginated)
    ListAssets (ListAssets'),
    newListAssets,
    ListAssetsResponse (ListAssetsResponse'),
    newListAssetsResponse,

    -- * Types

    -- ** AdMarkers
    AdMarkers (..),

    -- ** EncryptionMethod
    EncryptionMethod (..),

    -- ** ManifestLayout
    ManifestLayout (..),

    -- ** PeriodTriggersElement
    PeriodTriggersElement (..),

    -- ** Profile
    Profile (..),

    -- ** SegmentTemplateFormat
    SegmentTemplateFormat (..),

    -- ** StreamOrder
    StreamOrder (..),

    -- ** AssetShallow
    AssetShallow (AssetShallow'),
    newAssetShallow,

    -- ** Authorization
    Authorization (Authorization'),
    newAuthorization,

    -- ** CmafEncryption
    CmafEncryption (CmafEncryption'),
    newCmafEncryption,

    -- ** CmafPackage
    CmafPackage (CmafPackage'),
    newCmafPackage,

    -- ** DashEncryption
    DashEncryption (DashEncryption'),
    newDashEncryption,

    -- ** DashManifest
    DashManifest (DashManifest'),
    newDashManifest,

    -- ** DashPackage
    DashPackage (DashPackage'),
    newDashPackage,

    -- ** EgressAccessLogs
    EgressAccessLogs (EgressAccessLogs'),
    newEgressAccessLogs,

    -- ** EgressEndpoint
    EgressEndpoint (EgressEndpoint'),
    newEgressEndpoint,

    -- ** HlsEncryption
    HlsEncryption (HlsEncryption'),
    newHlsEncryption,

    -- ** HlsManifest
    HlsManifest (HlsManifest'),
    newHlsManifest,

    -- ** HlsPackage
    HlsPackage (HlsPackage'),
    newHlsPackage,

    -- ** MssEncryption
    MssEncryption (MssEncryption'),
    newMssEncryption,

    -- ** MssManifest
    MssManifest (MssManifest'),
    newMssManifest,

    -- ** MssPackage
    MssPackage (MssPackage'),
    newMssPackage,

    -- ** PackagingConfiguration
    PackagingConfiguration (PackagingConfiguration'),
    newPackagingConfiguration,

    -- ** PackagingGroup
    PackagingGroup (PackagingGroup'),
    newPackagingGroup,

    -- ** SpekeKeyProvider
    SpekeKeyProvider (SpekeKeyProvider'),
    newSpekeKeyProvider,

    -- ** StreamSelection
    StreamSelection (StreamSelection'),
    newStreamSelection,
  )
where

import Network.AWS.MediaPackageVOD.ConfigureLogs
import Network.AWS.MediaPackageVOD.CreateAsset
import Network.AWS.MediaPackageVOD.CreatePackagingConfiguration
import Network.AWS.MediaPackageVOD.CreatePackagingGroup
import Network.AWS.MediaPackageVOD.DeleteAsset
import Network.AWS.MediaPackageVOD.DeletePackagingConfiguration
import Network.AWS.MediaPackageVOD.DeletePackagingGroup
import Network.AWS.MediaPackageVOD.DescribeAsset
import Network.AWS.MediaPackageVOD.DescribePackagingConfiguration
import Network.AWS.MediaPackageVOD.DescribePackagingGroup
import Network.AWS.MediaPackageVOD.Lens
import Network.AWS.MediaPackageVOD.ListAssets
import Network.AWS.MediaPackageVOD.ListPackagingConfigurations
import Network.AWS.MediaPackageVOD.ListPackagingGroups
import Network.AWS.MediaPackageVOD.ListTagsForResource
import Network.AWS.MediaPackageVOD.TagResource
import Network.AWS.MediaPackageVOD.Types
import Network.AWS.MediaPackageVOD.UntagResource
import Network.AWS.MediaPackageVOD.UpdatePackagingGroup
import Network.AWS.MediaPackageVOD.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaPackageVOD'.

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
