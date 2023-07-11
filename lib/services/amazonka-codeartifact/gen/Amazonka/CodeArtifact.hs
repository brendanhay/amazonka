{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-09-22@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- CodeArtifact is a fully managed artifact repository compatible with
-- language-native package managers and build tools such as npm, Apache
-- Maven, pip, and dotnet. You can use CodeArtifact to share packages with
-- development teams and pull packages. Packages can be pulled from both
-- public and CodeArtifact repositories. You can also create an upstream
-- relationship between a CodeArtifact repository and another repository,
-- which effectively merges their contents from the point of view of a
-- package manager client.
--
-- __CodeArtifact Components__
--
-- Use the information in this guide to help you work with the following
-- CodeArtifact components:
--
-- -   __Repository__: A CodeArtifact repository contains a set of
--     <https://docs.aws.amazon.com/codeartifact/latest/ug/welcome.html#welcome-concepts-package-version package versions>,
--     each of which maps to a set of assets, or files. Repositories are
--     polyglot, so a single repository can contain packages of any
--     supported type. Each repository exposes endpoints for fetching and
--     publishing packages using tools like the __@npm@__ CLI, the Maven
--     CLI ( __@mvn@__ ), Python CLIs ( __@pip@__ and @twine@), and NuGet
--     CLIs (@nuget@ and @dotnet@).
--
-- -   __Domain__: Repositories are aggregated into a higher-level entity
--     known as a /domain/. All package assets and metadata are stored in
--     the domain, but are consumed through repositories. A given package
--     asset, such as a Maven JAR file, is stored once per domain, no
--     matter how many repositories it\'s present in. All of the assets and
--     metadata in a domain are encrypted with the same customer master key
--     (CMK) stored in Key Management Service (KMS).
--
--     Each repository is a member of a single domain and can\'t be moved
--     to a different domain.
--
--     The domain allows organizational policy to be applied across
--     multiple repositories, such as which accounts can access
--     repositories in the domain, and which public repositories can be
--     used as sources of packages.
--
--     Although an organization can have multiple domains, we recommend a
--     single production domain that contains all published artifacts so
--     that teams can find and share packages across their organization.
--
-- -   __Package__: A /package/ is a bundle of software and the metadata
--     required to resolve dependencies and install the software.
--     CodeArtifact supports
--     <https://docs.aws.amazon.com/codeartifact/latest/ug/using-npm.html npm>,
--     <https://docs.aws.amazon.com/codeartifact/latest/ug/using-python.html PyPI>,
--     <https://docs.aws.amazon.com/codeartifact/latest/ug/using-maven Maven>,
--     and
--     <https://docs.aws.amazon.com/codeartifact/latest/ug/using-nuget NuGet>
--     package formats.
--
--     In CodeArtifact, a package consists of:
--
--     -   A /name/ (for example, @webpack@ is the name of a popular npm
--         package)
--
--     -   An optional namespace (for example, @\@types@ in
--         @\@types\/node@)
--
--     -   A set of versions (for example, @1.0.0@, @1.0.1@, @1.0.2@, etc.)
--
--     -   Package-level metadata (for example, npm tags)
--
-- -   __Package version__: A version of a package, such as
--     @\@types\/node 12.6.9@. The version number format and semantics vary
--     for different package formats. For example, npm package versions
--     must conform to the
--     <https://semver.org/ Semantic Versioning specification>. In
--     CodeArtifact, a package version consists of the version identifier,
--     metadata at the package version level, and a set of assets.
--
-- -   __Upstream repository__: One repository is /upstream/ of another
--     when the package versions in it can be accessed from the repository
--     endpoint of the downstream repository, effectively merging the
--     contents of the two repositories from the point of view of a client.
--     CodeArtifact allows creating an upstream relationship between two
--     repositories.
--
-- -   __Asset__: An individual file stored in CodeArtifact associated with
--     a package version, such as an npm @.tgz@ file or Maven POM and JAR
--     files.
--
-- CodeArtifact supports these operations:
--
-- -   @AssociateExternalConnection@: Adds an existing external connection
--     to a repository.
--
-- -   @CopyPackageVersions@: Copies package versions from one repository
--     to another repository in the same domain.
--
-- -   @CreateDomain@: Creates a domain
--
-- -   @CreateRepository@: Creates a CodeArtifact repository in a domain.
--
-- -   @DeleteDomain@: Deletes a domain. You cannot delete a domain that
--     contains repositories.
--
-- -   @DeleteDomainPermissionsPolicy@: Deletes the resource policy that is
--     set on a domain.
--
-- -   @DeletePackageVersions@: Deletes versions of a package. After a
--     package has been deleted, it can be republished, but its assets and
--     metadata cannot be restored because they have been permanently
--     removed from storage.
--
-- -   @DeleteRepository@: Deletes a repository.
--
-- -   @DeleteRepositoryPermissionsPolicy@: Deletes the resource policy
--     that is set on a repository.
--
-- -   @DescribeDomain@: Returns a @DomainDescription@ object that contains
--     information about the requested domain.
--
-- -   @DescribePackage@: Returns a
--     <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDescription.html PackageDescription>
--     object that contains details about a package.
--
-- -   @DescribePackageVersion@: Returns a
--     <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionDescription.html PackageVersionDescription>
--     object that contains details about a package version.
--
-- -   @DescribeRepository@: Returns a @RepositoryDescription@ object that
--     contains detailed information about the requested repository.
--
-- -   @DisposePackageVersions@: Disposes versions of a package. A package
--     version with the status @Disposed@ cannot be restored because they
--     have been permanently removed from storage.
--
-- -   @DisassociateExternalConnection@: Removes an existing external
--     connection from a repository.
--
-- -   @GetAuthorizationToken@: Generates a temporary authorization token
--     for accessing repositories in the domain. The token expires the
--     authorization period has passed. The default authorization period is
--     12 hours and can be customized to any length with a maximum of 12
--     hours.
--
-- -   @GetDomainPermissionsPolicy@: Returns the policy of a resource that
--     is attached to the specified domain.
--
-- -   @GetPackageVersionAsset@: Returns the contents of an asset that is
--     in a package version.
--
-- -   @GetPackageVersionReadme@: Gets the readme file or descriptive text
--     for a package version.
--
-- -   @GetRepositoryEndpoint@: Returns the endpoint of a repository for a
--     specific package format. A repository has one endpoint for each
--     package format:
--
--     -   @maven@
--
--     -   @npm@
--
--     -   @nuget@
--
--     -   @pypi@
--
-- -   @GetRepositoryPermissionsPolicy@: Returns the resource policy that
--     is set on a repository.
--
-- -   @ListDomains@: Returns a list of @DomainSummary@ objects. Each
--     returned @DomainSummary@ object contains information about a domain.
--
-- -   @ListPackages@: Lists the packages in a repository.
--
-- -   @ListPackageVersionAssets@: Lists the assets for a given package
--     version.
--
-- -   @ListPackageVersionDependencies@: Returns a list of the direct
--     dependencies for a package version.
--
-- -   @ListPackageVersions@: Returns a list of package versions for a
--     specified package in a repository.
--
-- -   @ListRepositories@: Returns a list of repositories owned by the
--     Amazon Web Services account that called this method.
--
-- -   @ListRepositoriesInDomain@: Returns a list of the repositories in a
--     domain.
--
-- -   @PutDomainPermissionsPolicy@: Attaches a resource policy to a
--     domain.
--
-- -   @PutPackageOriginConfiguration@: Sets the package origin
--     configuration for a package, which determine how new versions of the
--     package can be added to a specific repository.
--
-- -   @PutRepositoryPermissionsPolicy@: Sets the resource policy on a
--     repository that specifies permissions to access it.
--
-- -   @UpdatePackageVersionsStatus@: Updates the status of one or more
--     versions of a package.
--
-- -   @UpdateRepository@: Updates the properties of a repository.
module Amazonka.CodeArtifact
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateExternalConnection
    AssociateExternalConnection (AssociateExternalConnection'),
    newAssociateExternalConnection,
    AssociateExternalConnectionResponse (AssociateExternalConnectionResponse'),
    newAssociateExternalConnectionResponse,

    -- ** CopyPackageVersions
    CopyPackageVersions (CopyPackageVersions'),
    newCopyPackageVersions,
    CopyPackageVersionsResponse (CopyPackageVersionsResponse'),
    newCopyPackageVersionsResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** CreateRepository
    CreateRepository (CreateRepository'),
    newCreateRepository,
    CreateRepositoryResponse (CreateRepositoryResponse'),
    newCreateRepositoryResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteDomainPermissionsPolicy
    DeleteDomainPermissionsPolicy (DeleteDomainPermissionsPolicy'),
    newDeleteDomainPermissionsPolicy,
    DeleteDomainPermissionsPolicyResponse (DeleteDomainPermissionsPolicyResponse'),
    newDeleteDomainPermissionsPolicyResponse,

    -- ** DeletePackageVersions
    DeletePackageVersions (DeletePackageVersions'),
    newDeletePackageVersions,
    DeletePackageVersionsResponse (DeletePackageVersionsResponse'),
    newDeletePackageVersionsResponse,

    -- ** DeleteRepository
    DeleteRepository (DeleteRepository'),
    newDeleteRepository,
    DeleteRepositoryResponse (DeleteRepositoryResponse'),
    newDeleteRepositoryResponse,

    -- ** DeleteRepositoryPermissionsPolicy
    DeleteRepositoryPermissionsPolicy (DeleteRepositoryPermissionsPolicy'),
    newDeleteRepositoryPermissionsPolicy,
    DeleteRepositoryPermissionsPolicyResponse (DeleteRepositoryPermissionsPolicyResponse'),
    newDeleteRepositoryPermissionsPolicyResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** DescribePackage
    DescribePackage (DescribePackage'),
    newDescribePackage,
    DescribePackageResponse (DescribePackageResponse'),
    newDescribePackageResponse,

    -- ** DescribePackageVersion
    DescribePackageVersion (DescribePackageVersion'),
    newDescribePackageVersion,
    DescribePackageVersionResponse (DescribePackageVersionResponse'),
    newDescribePackageVersionResponse,

    -- ** DescribeRepository
    DescribeRepository (DescribeRepository'),
    newDescribeRepository,
    DescribeRepositoryResponse (DescribeRepositoryResponse'),
    newDescribeRepositoryResponse,

    -- ** DisassociateExternalConnection
    DisassociateExternalConnection (DisassociateExternalConnection'),
    newDisassociateExternalConnection,
    DisassociateExternalConnectionResponse (DisassociateExternalConnectionResponse'),
    newDisassociateExternalConnectionResponse,

    -- ** DisposePackageVersions
    DisposePackageVersions (DisposePackageVersions'),
    newDisposePackageVersions,
    DisposePackageVersionsResponse (DisposePackageVersionsResponse'),
    newDisposePackageVersionsResponse,

    -- ** GetAuthorizationToken
    GetAuthorizationToken (GetAuthorizationToken'),
    newGetAuthorizationToken,
    GetAuthorizationTokenResponse (GetAuthorizationTokenResponse'),
    newGetAuthorizationTokenResponse,

    -- ** GetDomainPermissionsPolicy
    GetDomainPermissionsPolicy (GetDomainPermissionsPolicy'),
    newGetDomainPermissionsPolicy,
    GetDomainPermissionsPolicyResponse (GetDomainPermissionsPolicyResponse'),
    newGetDomainPermissionsPolicyResponse,

    -- ** GetPackageVersionAsset
    GetPackageVersionAsset (GetPackageVersionAsset'),
    newGetPackageVersionAsset,
    GetPackageVersionAssetResponse (GetPackageVersionAssetResponse'),
    newGetPackageVersionAssetResponse,

    -- ** GetPackageVersionReadme
    GetPackageVersionReadme (GetPackageVersionReadme'),
    newGetPackageVersionReadme,
    GetPackageVersionReadmeResponse (GetPackageVersionReadmeResponse'),
    newGetPackageVersionReadmeResponse,

    -- ** GetRepositoryEndpoint
    GetRepositoryEndpoint (GetRepositoryEndpoint'),
    newGetRepositoryEndpoint,
    GetRepositoryEndpointResponse (GetRepositoryEndpointResponse'),
    newGetRepositoryEndpointResponse,

    -- ** GetRepositoryPermissionsPolicy
    GetRepositoryPermissionsPolicy (GetRepositoryPermissionsPolicy'),
    newGetRepositoryPermissionsPolicy,
    GetRepositoryPermissionsPolicyResponse (GetRepositoryPermissionsPolicyResponse'),
    newGetRepositoryPermissionsPolicyResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** ListPackageVersionAssets (Paginated)
    ListPackageVersionAssets (ListPackageVersionAssets'),
    newListPackageVersionAssets,
    ListPackageVersionAssetsResponse (ListPackageVersionAssetsResponse'),
    newListPackageVersionAssetsResponse,

    -- ** ListPackageVersionDependencies
    ListPackageVersionDependencies (ListPackageVersionDependencies'),
    newListPackageVersionDependencies,
    ListPackageVersionDependenciesResponse (ListPackageVersionDependenciesResponse'),
    newListPackageVersionDependenciesResponse,

    -- ** ListPackageVersions (Paginated)
    ListPackageVersions (ListPackageVersions'),
    newListPackageVersions,
    ListPackageVersionsResponse (ListPackageVersionsResponse'),
    newListPackageVersionsResponse,

    -- ** ListPackages (Paginated)
    ListPackages (ListPackages'),
    newListPackages,
    ListPackagesResponse (ListPackagesResponse'),
    newListPackagesResponse,

    -- ** ListRepositories (Paginated)
    ListRepositories (ListRepositories'),
    newListRepositories,
    ListRepositoriesResponse (ListRepositoriesResponse'),
    newListRepositoriesResponse,

    -- ** ListRepositoriesInDomain (Paginated)
    ListRepositoriesInDomain (ListRepositoriesInDomain'),
    newListRepositoriesInDomain,
    ListRepositoriesInDomainResponse (ListRepositoriesInDomainResponse'),
    newListRepositoriesInDomainResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutDomainPermissionsPolicy
    PutDomainPermissionsPolicy (PutDomainPermissionsPolicy'),
    newPutDomainPermissionsPolicy,
    PutDomainPermissionsPolicyResponse (PutDomainPermissionsPolicyResponse'),
    newPutDomainPermissionsPolicyResponse,

    -- ** PutPackageOriginConfiguration
    PutPackageOriginConfiguration (PutPackageOriginConfiguration'),
    newPutPackageOriginConfiguration,
    PutPackageOriginConfigurationResponse (PutPackageOriginConfigurationResponse'),
    newPutPackageOriginConfigurationResponse,

    -- ** PutRepositoryPermissionsPolicy
    PutRepositoryPermissionsPolicy (PutRepositoryPermissionsPolicy'),
    newPutRepositoryPermissionsPolicy,
    PutRepositoryPermissionsPolicyResponse (PutRepositoryPermissionsPolicyResponse'),
    newPutRepositoryPermissionsPolicyResponse,

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

    -- ** UpdatePackageVersionsStatus
    UpdatePackageVersionsStatus (UpdatePackageVersionsStatus'),
    newUpdatePackageVersionsStatus,
    UpdatePackageVersionsStatusResponse (UpdatePackageVersionsStatusResponse'),
    newUpdatePackageVersionsStatusResponse,

    -- ** UpdateRepository
    UpdateRepository (UpdateRepository'),
    newUpdateRepository,
    UpdateRepositoryResponse (UpdateRepositoryResponse'),
    newUpdateRepositoryResponse,

    -- * Types

    -- ** AllowPublish
    AllowPublish (..),

    -- ** AllowUpstream
    AllowUpstream (..),

    -- ** DomainStatus
    DomainStatus (..),

    -- ** ExternalConnectionStatus
    ExternalConnectionStatus (..),

    -- ** HashAlgorithm
    HashAlgorithm (..),

    -- ** PackageFormat
    PackageFormat (..),

    -- ** PackageVersionErrorCode
    PackageVersionErrorCode (..),

    -- ** PackageVersionOriginType
    PackageVersionOriginType (..),

    -- ** PackageVersionSortType
    PackageVersionSortType (..),

    -- ** PackageVersionStatus
    PackageVersionStatus (..),

    -- ** AssetSummary
    AssetSummary (AssetSummary'),
    newAssetSummary,

    -- ** DomainDescription
    DomainDescription (DomainDescription'),
    newDomainDescription,

    -- ** DomainEntryPoint
    DomainEntryPoint (DomainEntryPoint'),
    newDomainEntryPoint,

    -- ** DomainSummary
    DomainSummary (DomainSummary'),
    newDomainSummary,

    -- ** LicenseInfo
    LicenseInfo (LicenseInfo'),
    newLicenseInfo,

    -- ** PackageDependency
    PackageDependency (PackageDependency'),
    newPackageDependency,

    -- ** PackageDescription
    PackageDescription (PackageDescription'),
    newPackageDescription,

    -- ** PackageOriginConfiguration
    PackageOriginConfiguration (PackageOriginConfiguration'),
    newPackageOriginConfiguration,

    -- ** PackageOriginRestrictions
    PackageOriginRestrictions (PackageOriginRestrictions'),
    newPackageOriginRestrictions,

    -- ** PackageSummary
    PackageSummary (PackageSummary'),
    newPackageSummary,

    -- ** PackageVersionDescription
    PackageVersionDescription (PackageVersionDescription'),
    newPackageVersionDescription,

    -- ** PackageVersionError
    PackageVersionError (PackageVersionError'),
    newPackageVersionError,

    -- ** PackageVersionOrigin
    PackageVersionOrigin (PackageVersionOrigin'),
    newPackageVersionOrigin,

    -- ** PackageVersionSummary
    PackageVersionSummary (PackageVersionSummary'),
    newPackageVersionSummary,

    -- ** RepositoryDescription
    RepositoryDescription (RepositoryDescription'),
    newRepositoryDescription,

    -- ** RepositoryExternalConnectionInfo
    RepositoryExternalConnectionInfo (RepositoryExternalConnectionInfo'),
    newRepositoryExternalConnectionInfo,

    -- ** RepositorySummary
    RepositorySummary (RepositorySummary'),
    newRepositorySummary,

    -- ** ResourcePolicy
    ResourcePolicy (ResourcePolicy'),
    newResourcePolicy,

    -- ** SuccessfulPackageVersionInfo
    SuccessfulPackageVersionInfo (SuccessfulPackageVersionInfo'),
    newSuccessfulPackageVersionInfo,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UpstreamRepository
    UpstreamRepository (UpstreamRepository'),
    newUpstreamRepository,

    -- ** UpstreamRepositoryInfo
    UpstreamRepositoryInfo (UpstreamRepositoryInfo'),
    newUpstreamRepositoryInfo,
  )
where

import Amazonka.CodeArtifact.AssociateExternalConnection
import Amazonka.CodeArtifact.CopyPackageVersions
import Amazonka.CodeArtifact.CreateDomain
import Amazonka.CodeArtifact.CreateRepository
import Amazonka.CodeArtifact.DeleteDomain
import Amazonka.CodeArtifact.DeleteDomainPermissionsPolicy
import Amazonka.CodeArtifact.DeletePackageVersions
import Amazonka.CodeArtifact.DeleteRepository
import Amazonka.CodeArtifact.DeleteRepositoryPermissionsPolicy
import Amazonka.CodeArtifact.DescribeDomain
import Amazonka.CodeArtifact.DescribePackage
import Amazonka.CodeArtifact.DescribePackageVersion
import Amazonka.CodeArtifact.DescribeRepository
import Amazonka.CodeArtifact.DisassociateExternalConnection
import Amazonka.CodeArtifact.DisposePackageVersions
import Amazonka.CodeArtifact.GetAuthorizationToken
import Amazonka.CodeArtifact.GetDomainPermissionsPolicy
import Amazonka.CodeArtifact.GetPackageVersionAsset
import Amazonka.CodeArtifact.GetPackageVersionReadme
import Amazonka.CodeArtifact.GetRepositoryEndpoint
import Amazonka.CodeArtifact.GetRepositoryPermissionsPolicy
import Amazonka.CodeArtifact.Lens
import Amazonka.CodeArtifact.ListDomains
import Amazonka.CodeArtifact.ListPackageVersionAssets
import Amazonka.CodeArtifact.ListPackageVersionDependencies
import Amazonka.CodeArtifact.ListPackageVersions
import Amazonka.CodeArtifact.ListPackages
import Amazonka.CodeArtifact.ListRepositories
import Amazonka.CodeArtifact.ListRepositoriesInDomain
import Amazonka.CodeArtifact.ListTagsForResource
import Amazonka.CodeArtifact.PutDomainPermissionsPolicy
import Amazonka.CodeArtifact.PutPackageOriginConfiguration
import Amazonka.CodeArtifact.PutRepositoryPermissionsPolicy
import Amazonka.CodeArtifact.TagResource
import Amazonka.CodeArtifact.Types
import Amazonka.CodeArtifact.UntagResource
import Amazonka.CodeArtifact.UpdatePackageVersionsStatus
import Amazonka.CodeArtifact.UpdateRepository
import Amazonka.CodeArtifact.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeArtifact'.

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
