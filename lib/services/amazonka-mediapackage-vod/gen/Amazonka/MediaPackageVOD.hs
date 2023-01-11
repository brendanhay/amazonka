{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaPackageVOD
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-07@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Elemental MediaPackage VOD
module Amazonka.MediaPackageVOD
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ConfigureLogs
    ConfigureLogs (ConfigureLogs'),
    newConfigureLogs,
    ConfigureLogsResponse (ConfigureLogsResponse'),
    newConfigureLogsResponse,

    -- ** CreateAsset
    CreateAsset (CreateAsset'),
    newCreateAsset,
    CreateAssetResponse (CreateAssetResponse'),
    newCreateAssetResponse,

    -- ** CreatePackagingConfiguration
    CreatePackagingConfiguration (CreatePackagingConfiguration'),
    newCreatePackagingConfiguration,
    CreatePackagingConfigurationResponse (CreatePackagingConfigurationResponse'),
    newCreatePackagingConfigurationResponse,

    -- ** CreatePackagingGroup
    CreatePackagingGroup (CreatePackagingGroup'),
    newCreatePackagingGroup,
    CreatePackagingGroupResponse (CreatePackagingGroupResponse'),
    newCreatePackagingGroupResponse,

    -- ** DeleteAsset
    DeleteAsset (DeleteAsset'),
    newDeleteAsset,
    DeleteAssetResponse (DeleteAssetResponse'),
    newDeleteAssetResponse,

    -- ** DeletePackagingConfiguration
    DeletePackagingConfiguration (DeletePackagingConfiguration'),
    newDeletePackagingConfiguration,
    DeletePackagingConfigurationResponse (DeletePackagingConfigurationResponse'),
    newDeletePackagingConfigurationResponse,

    -- ** DeletePackagingGroup
    DeletePackagingGroup (DeletePackagingGroup'),
    newDeletePackagingGroup,
    DeletePackagingGroupResponse (DeletePackagingGroupResponse'),
    newDeletePackagingGroupResponse,

    -- ** DescribeAsset
    DescribeAsset (DescribeAsset'),
    newDescribeAsset,
    DescribeAssetResponse (DescribeAssetResponse'),
    newDescribeAssetResponse,

    -- ** DescribePackagingConfiguration
    DescribePackagingConfiguration (DescribePackagingConfiguration'),
    newDescribePackagingConfiguration,
    DescribePackagingConfigurationResponse (DescribePackagingConfigurationResponse'),
    newDescribePackagingConfigurationResponse,

    -- ** DescribePackagingGroup
    DescribePackagingGroup (DescribePackagingGroup'),
    newDescribePackagingGroup,
    DescribePackagingGroupResponse (DescribePackagingGroupResponse'),
    newDescribePackagingGroupResponse,

    -- ** ListAssets (Paginated)
    ListAssets (ListAssets'),
    newListAssets,
    ListAssetsResponse (ListAssetsResponse'),
    newListAssetsResponse,

    -- ** ListPackagingConfigurations (Paginated)
    ListPackagingConfigurations (ListPackagingConfigurations'),
    newListPackagingConfigurations,
    ListPackagingConfigurationsResponse (ListPackagingConfigurationsResponse'),
    newListPackagingConfigurationsResponse,

    -- ** ListPackagingGroups (Paginated)
    ListPackagingGroups (ListPackagingGroups'),
    newListPackagingGroups,
    ListPackagingGroupsResponse (ListPackagingGroupsResponse'),
    newListPackagingGroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdatePackagingGroup
    UpdatePackagingGroup (UpdatePackagingGroup'),
    newUpdatePackagingGroup,
    UpdatePackagingGroupResponse (UpdatePackagingGroupResponse'),
    newUpdatePackagingGroupResponse,

    -- * Types

    -- ** AdMarkers
    AdMarkers (..),

    -- ** EncryptionMethod
    EncryptionMethod (..),

    -- ** ManifestLayout
    ManifestLayout (..),

    -- ** PeriodTriggersElement
    PeriodTriggersElement (..),

    -- ** PresetSpeke20Audio
    PresetSpeke20Audio (..),

    -- ** PresetSpeke20Video
    PresetSpeke20Video (..),

    -- ** Profile
    Profile (..),

    -- ** ScteMarkersSource
    ScteMarkersSource (..),

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

    -- ** EncryptionContractConfiguration
    EncryptionContractConfiguration (EncryptionContractConfiguration'),
    newEncryptionContractConfiguration,

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

import Amazonka.MediaPackageVOD.ConfigureLogs
import Amazonka.MediaPackageVOD.CreateAsset
import Amazonka.MediaPackageVOD.CreatePackagingConfiguration
import Amazonka.MediaPackageVOD.CreatePackagingGroup
import Amazonka.MediaPackageVOD.DeleteAsset
import Amazonka.MediaPackageVOD.DeletePackagingConfiguration
import Amazonka.MediaPackageVOD.DeletePackagingGroup
import Amazonka.MediaPackageVOD.DescribeAsset
import Amazonka.MediaPackageVOD.DescribePackagingConfiguration
import Amazonka.MediaPackageVOD.DescribePackagingGroup
import Amazonka.MediaPackageVOD.Lens
import Amazonka.MediaPackageVOD.ListAssets
import Amazonka.MediaPackageVOD.ListPackagingConfigurations
import Amazonka.MediaPackageVOD.ListPackagingGroups
import Amazonka.MediaPackageVOD.ListTagsForResource
import Amazonka.MediaPackageVOD.TagResource
import Amazonka.MediaPackageVOD.Types
import Amazonka.MediaPackageVOD.UntagResource
import Amazonka.MediaPackageVOD.UpdatePackagingGroup
import Amazonka.MediaPackageVOD.Waiters

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
