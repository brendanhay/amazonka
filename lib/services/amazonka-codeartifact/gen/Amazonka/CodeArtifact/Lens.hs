{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeArtifact.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Lens
  ( -- * Operations

    -- ** AssociateExternalConnection
    associateExternalConnection_domainOwner,
    associateExternalConnection_domain,
    associateExternalConnection_repository,
    associateExternalConnection_externalConnection,
    associateExternalConnectionResponse_repository,
    associateExternalConnectionResponse_httpStatus,

    -- ** CopyPackageVersions
    copyPackageVersions_allowOverwrite,
    copyPackageVersions_domainOwner,
    copyPackageVersions_includeFromUpstream,
    copyPackageVersions_namespace,
    copyPackageVersions_versionRevisions,
    copyPackageVersions_versions,
    copyPackageVersions_domain,
    copyPackageVersions_sourceRepository,
    copyPackageVersions_destinationRepository,
    copyPackageVersions_format,
    copyPackageVersions_package,
    copyPackageVersionsResponse_failedVersions,
    copyPackageVersionsResponse_successfulVersions,
    copyPackageVersionsResponse_httpStatus,

    -- ** CreateDomain
    createDomain_encryptionKey,
    createDomain_tags,
    createDomain_domain,
    createDomainResponse_domain,
    createDomainResponse_httpStatus,

    -- ** CreateRepository
    createRepository_description,
    createRepository_domainOwner,
    createRepository_tags,
    createRepository_upstreams,
    createRepository_domain,
    createRepository_repository,
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainOwner,
    deleteDomain_domain,
    deleteDomainResponse_domain,
    deleteDomainResponse_httpStatus,

    -- ** DeleteDomainPermissionsPolicy
    deleteDomainPermissionsPolicy_domainOwner,
    deleteDomainPermissionsPolicy_policyRevision,
    deleteDomainPermissionsPolicy_domain,
    deleteDomainPermissionsPolicyResponse_policy,
    deleteDomainPermissionsPolicyResponse_httpStatus,

    -- ** DeletePackageVersions
    deletePackageVersions_domainOwner,
    deletePackageVersions_expectedStatus,
    deletePackageVersions_namespace,
    deletePackageVersions_domain,
    deletePackageVersions_repository,
    deletePackageVersions_format,
    deletePackageVersions_package,
    deletePackageVersions_versions,
    deletePackageVersionsResponse_failedVersions,
    deletePackageVersionsResponse_successfulVersions,
    deletePackageVersionsResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_domainOwner,
    deleteRepository_domain,
    deleteRepository_repository,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** DeleteRepositoryPermissionsPolicy
    deleteRepositoryPermissionsPolicy_domainOwner,
    deleteRepositoryPermissionsPolicy_policyRevision,
    deleteRepositoryPermissionsPolicy_domain,
    deleteRepositoryPermissionsPolicy_repository,
    deleteRepositoryPermissionsPolicyResponse_policy,
    deleteRepositoryPermissionsPolicyResponse_httpStatus,

    -- ** DescribeDomain
    describeDomain_domainOwner,
    describeDomain_domain,
    describeDomainResponse_domain,
    describeDomainResponse_httpStatus,

    -- ** DescribePackage
    describePackage_domainOwner,
    describePackage_namespace,
    describePackage_domain,
    describePackage_repository,
    describePackage_format,
    describePackage_package,
    describePackageResponse_httpStatus,
    describePackageResponse_package,

    -- ** DescribePackageVersion
    describePackageVersion_domainOwner,
    describePackageVersion_namespace,
    describePackageVersion_domain,
    describePackageVersion_repository,
    describePackageVersion_format,
    describePackageVersion_package,
    describePackageVersion_packageVersion,
    describePackageVersionResponse_httpStatus,
    describePackageVersionResponse_packageVersion,

    -- ** DescribeRepository
    describeRepository_domainOwner,
    describeRepository_domain,
    describeRepository_repository,
    describeRepositoryResponse_repository,
    describeRepositoryResponse_httpStatus,

    -- ** DisassociateExternalConnection
    disassociateExternalConnection_domainOwner,
    disassociateExternalConnection_domain,
    disassociateExternalConnection_repository,
    disassociateExternalConnection_externalConnection,
    disassociateExternalConnectionResponse_repository,
    disassociateExternalConnectionResponse_httpStatus,

    -- ** DisposePackageVersions
    disposePackageVersions_domainOwner,
    disposePackageVersions_expectedStatus,
    disposePackageVersions_namespace,
    disposePackageVersions_versionRevisions,
    disposePackageVersions_domain,
    disposePackageVersions_repository,
    disposePackageVersions_format,
    disposePackageVersions_package,
    disposePackageVersions_versions,
    disposePackageVersionsResponse_failedVersions,
    disposePackageVersionsResponse_successfulVersions,
    disposePackageVersionsResponse_httpStatus,

    -- ** GetAuthorizationToken
    getAuthorizationToken_domainOwner,
    getAuthorizationToken_durationSeconds,
    getAuthorizationToken_domain,
    getAuthorizationTokenResponse_authorizationToken,
    getAuthorizationTokenResponse_expiration,
    getAuthorizationTokenResponse_httpStatus,

    -- ** GetDomainPermissionsPolicy
    getDomainPermissionsPolicy_domainOwner,
    getDomainPermissionsPolicy_domain,
    getDomainPermissionsPolicyResponse_policy,
    getDomainPermissionsPolicyResponse_httpStatus,

    -- ** GetPackageVersionAsset
    getPackageVersionAsset_domainOwner,
    getPackageVersionAsset_namespace,
    getPackageVersionAsset_packageVersionRevision,
    getPackageVersionAsset_domain,
    getPackageVersionAsset_repository,
    getPackageVersionAsset_format,
    getPackageVersionAsset_package,
    getPackageVersionAsset_packageVersion,
    getPackageVersionAsset_asset,
    getPackageVersionAssetResponse_assetName,
    getPackageVersionAssetResponse_packageVersion,
    getPackageVersionAssetResponse_packageVersionRevision,
    getPackageVersionAssetResponse_httpStatus,
    getPackageVersionAssetResponse_asset,

    -- ** GetPackageVersionReadme
    getPackageVersionReadme_domainOwner,
    getPackageVersionReadme_namespace,
    getPackageVersionReadme_domain,
    getPackageVersionReadme_repository,
    getPackageVersionReadme_format,
    getPackageVersionReadme_package,
    getPackageVersionReadme_packageVersion,
    getPackageVersionReadmeResponse_format,
    getPackageVersionReadmeResponse_namespace,
    getPackageVersionReadmeResponse_package,
    getPackageVersionReadmeResponse_readme,
    getPackageVersionReadmeResponse_version,
    getPackageVersionReadmeResponse_versionRevision,
    getPackageVersionReadmeResponse_httpStatus,

    -- ** GetRepositoryEndpoint
    getRepositoryEndpoint_domainOwner,
    getRepositoryEndpoint_domain,
    getRepositoryEndpoint_repository,
    getRepositoryEndpoint_format,
    getRepositoryEndpointResponse_repositoryEndpoint,
    getRepositoryEndpointResponse_httpStatus,

    -- ** GetRepositoryPermissionsPolicy
    getRepositoryPermissionsPolicy_domainOwner,
    getRepositoryPermissionsPolicy_domain,
    getRepositoryPermissionsPolicy_repository,
    getRepositoryPermissionsPolicyResponse_policy,
    getRepositoryPermissionsPolicyResponse_httpStatus,

    -- ** ListDomains
    listDomains_maxResults,
    listDomains_nextToken,
    listDomainsResponse_domains,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- ** ListPackageVersionAssets
    listPackageVersionAssets_domainOwner,
    listPackageVersionAssets_maxResults,
    listPackageVersionAssets_namespace,
    listPackageVersionAssets_nextToken,
    listPackageVersionAssets_domain,
    listPackageVersionAssets_repository,
    listPackageVersionAssets_format,
    listPackageVersionAssets_package,
    listPackageVersionAssets_packageVersion,
    listPackageVersionAssetsResponse_assets,
    listPackageVersionAssetsResponse_format,
    listPackageVersionAssetsResponse_namespace,
    listPackageVersionAssetsResponse_nextToken,
    listPackageVersionAssetsResponse_package,
    listPackageVersionAssetsResponse_version,
    listPackageVersionAssetsResponse_versionRevision,
    listPackageVersionAssetsResponse_httpStatus,

    -- ** ListPackageVersionDependencies
    listPackageVersionDependencies_domainOwner,
    listPackageVersionDependencies_namespace,
    listPackageVersionDependencies_nextToken,
    listPackageVersionDependencies_domain,
    listPackageVersionDependencies_repository,
    listPackageVersionDependencies_format,
    listPackageVersionDependencies_package,
    listPackageVersionDependencies_packageVersion,
    listPackageVersionDependenciesResponse_dependencies,
    listPackageVersionDependenciesResponse_format,
    listPackageVersionDependenciesResponse_namespace,
    listPackageVersionDependenciesResponse_nextToken,
    listPackageVersionDependenciesResponse_package,
    listPackageVersionDependenciesResponse_version,
    listPackageVersionDependenciesResponse_versionRevision,
    listPackageVersionDependenciesResponse_httpStatus,

    -- ** ListPackageVersions
    listPackageVersions_domainOwner,
    listPackageVersions_maxResults,
    listPackageVersions_namespace,
    listPackageVersions_nextToken,
    listPackageVersions_originType,
    listPackageVersions_sortBy,
    listPackageVersions_status,
    listPackageVersions_domain,
    listPackageVersions_repository,
    listPackageVersions_format,
    listPackageVersions_package,
    listPackageVersionsResponse_defaultDisplayVersion,
    listPackageVersionsResponse_format,
    listPackageVersionsResponse_namespace,
    listPackageVersionsResponse_nextToken,
    listPackageVersionsResponse_package,
    listPackageVersionsResponse_versions,
    listPackageVersionsResponse_httpStatus,

    -- ** ListPackages
    listPackages_domainOwner,
    listPackages_format,
    listPackages_maxResults,
    listPackages_namespace,
    listPackages_nextToken,
    listPackages_packagePrefix,
    listPackages_publish,
    listPackages_upstream,
    listPackages_domain,
    listPackages_repository,
    listPackagesResponse_nextToken,
    listPackagesResponse_packages,
    listPackagesResponse_httpStatus,

    -- ** ListRepositories
    listRepositories_maxResults,
    listRepositories_nextToken,
    listRepositories_repositoryPrefix,
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_httpStatus,

    -- ** ListRepositoriesInDomain
    listRepositoriesInDomain_administratorAccount,
    listRepositoriesInDomain_domainOwner,
    listRepositoriesInDomain_maxResults,
    listRepositoriesInDomain_nextToken,
    listRepositoriesInDomain_repositoryPrefix,
    listRepositoriesInDomain_domain,
    listRepositoriesInDomainResponse_nextToken,
    listRepositoriesInDomainResponse_repositories,
    listRepositoriesInDomainResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutDomainPermissionsPolicy
    putDomainPermissionsPolicy_domainOwner,
    putDomainPermissionsPolicy_policyRevision,
    putDomainPermissionsPolicy_domain,
    putDomainPermissionsPolicy_policyDocument,
    putDomainPermissionsPolicyResponse_policy,
    putDomainPermissionsPolicyResponse_httpStatus,

    -- ** PutPackageOriginConfiguration
    putPackageOriginConfiguration_domainOwner,
    putPackageOriginConfiguration_namespace,
    putPackageOriginConfiguration_domain,
    putPackageOriginConfiguration_repository,
    putPackageOriginConfiguration_format,
    putPackageOriginConfiguration_package,
    putPackageOriginConfiguration_restrictions,
    putPackageOriginConfigurationResponse_originConfiguration,
    putPackageOriginConfigurationResponse_httpStatus,

    -- ** PutRepositoryPermissionsPolicy
    putRepositoryPermissionsPolicy_domainOwner,
    putRepositoryPermissionsPolicy_policyRevision,
    putRepositoryPermissionsPolicy_domain,
    putRepositoryPermissionsPolicy_repository,
    putRepositoryPermissionsPolicy_policyDocument,
    putRepositoryPermissionsPolicyResponse_policy,
    putRepositoryPermissionsPolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdatePackageVersionsStatus
    updatePackageVersionsStatus_domainOwner,
    updatePackageVersionsStatus_expectedStatus,
    updatePackageVersionsStatus_namespace,
    updatePackageVersionsStatus_versionRevisions,
    updatePackageVersionsStatus_domain,
    updatePackageVersionsStatus_repository,
    updatePackageVersionsStatus_format,
    updatePackageVersionsStatus_package,
    updatePackageVersionsStatus_versions,
    updatePackageVersionsStatus_targetStatus,
    updatePackageVersionsStatusResponse_failedVersions,
    updatePackageVersionsStatusResponse_successfulVersions,
    updatePackageVersionsStatusResponse_httpStatus,

    -- ** UpdateRepository
    updateRepository_description,
    updateRepository_domainOwner,
    updateRepository_upstreams,
    updateRepository_domain,
    updateRepository_repository,
    updateRepositoryResponse_repository,
    updateRepositoryResponse_httpStatus,

    -- * Types

    -- ** AssetSummary
    assetSummary_hashes,
    assetSummary_size,
    assetSummary_name,

    -- ** DomainDescription
    domainDescription_arn,
    domainDescription_assetSizeBytes,
    domainDescription_createdTime,
    domainDescription_encryptionKey,
    domainDescription_name,
    domainDescription_owner,
    domainDescription_repositoryCount,
    domainDescription_s3BucketArn,
    domainDescription_status,

    -- ** DomainEntryPoint
    domainEntryPoint_externalConnectionName,
    domainEntryPoint_repositoryName,

    -- ** DomainSummary
    domainSummary_arn,
    domainSummary_createdTime,
    domainSummary_encryptionKey,
    domainSummary_name,
    domainSummary_owner,
    domainSummary_status,

    -- ** LicenseInfo
    licenseInfo_name,
    licenseInfo_url,

    -- ** PackageDependency
    packageDependency_dependencyType,
    packageDependency_namespace,
    packageDependency_package,
    packageDependency_versionRequirement,

    -- ** PackageDescription
    packageDescription_format,
    packageDescription_name,
    packageDescription_namespace,
    packageDescription_originConfiguration,

    -- ** PackageOriginConfiguration
    packageOriginConfiguration_restrictions,

    -- ** PackageOriginRestrictions
    packageOriginRestrictions_publish,
    packageOriginRestrictions_upstream,

    -- ** PackageSummary
    packageSummary_format,
    packageSummary_namespace,
    packageSummary_originConfiguration,
    packageSummary_package,

    -- ** PackageVersionDescription
    packageVersionDescription_displayName,
    packageVersionDescription_format,
    packageVersionDescription_homePage,
    packageVersionDescription_licenses,
    packageVersionDescription_namespace,
    packageVersionDescription_origin,
    packageVersionDescription_packageName,
    packageVersionDescription_publishedTime,
    packageVersionDescription_revision,
    packageVersionDescription_sourceCodeRepository,
    packageVersionDescription_status,
    packageVersionDescription_summary,
    packageVersionDescription_version,

    -- ** PackageVersionError
    packageVersionError_errorCode,
    packageVersionError_errorMessage,

    -- ** PackageVersionOrigin
    packageVersionOrigin_domainEntryPoint,
    packageVersionOrigin_originType,

    -- ** PackageVersionSummary
    packageVersionSummary_origin,
    packageVersionSummary_revision,
    packageVersionSummary_version,
    packageVersionSummary_status,

    -- ** RepositoryDescription
    repositoryDescription_administratorAccount,
    repositoryDescription_arn,
    repositoryDescription_description,
    repositoryDescription_domainName,
    repositoryDescription_domainOwner,
    repositoryDescription_externalConnections,
    repositoryDescription_name,
    repositoryDescription_upstreams,

    -- ** RepositoryExternalConnectionInfo
    repositoryExternalConnectionInfo_externalConnectionName,
    repositoryExternalConnectionInfo_packageFormat,
    repositoryExternalConnectionInfo_status,

    -- ** RepositorySummary
    repositorySummary_administratorAccount,
    repositorySummary_arn,
    repositorySummary_description,
    repositorySummary_domainName,
    repositorySummary_domainOwner,
    repositorySummary_name,

    -- ** ResourcePolicy
    resourcePolicy_document,
    resourcePolicy_resourceArn,
    resourcePolicy_revision,

    -- ** SuccessfulPackageVersionInfo
    successfulPackageVersionInfo_revision,
    successfulPackageVersionInfo_status,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpstreamRepository
    upstreamRepository_repositoryName,

    -- ** UpstreamRepositoryInfo
    upstreamRepositoryInfo_repositoryName,
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
import Amazonka.CodeArtifact.Types.AssetSummary
import Amazonka.CodeArtifact.Types.DomainDescription
import Amazonka.CodeArtifact.Types.DomainEntryPoint
import Amazonka.CodeArtifact.Types.DomainSummary
import Amazonka.CodeArtifact.Types.LicenseInfo
import Amazonka.CodeArtifact.Types.PackageDependency
import Amazonka.CodeArtifact.Types.PackageDescription
import Amazonka.CodeArtifact.Types.PackageOriginConfiguration
import Amazonka.CodeArtifact.Types.PackageOriginRestrictions
import Amazonka.CodeArtifact.Types.PackageSummary
import Amazonka.CodeArtifact.Types.PackageVersionDescription
import Amazonka.CodeArtifact.Types.PackageVersionError
import Amazonka.CodeArtifact.Types.PackageVersionOrigin
import Amazonka.CodeArtifact.Types.PackageVersionSummary
import Amazonka.CodeArtifact.Types.RepositoryDescription
import Amazonka.CodeArtifact.Types.RepositoryExternalConnectionInfo
import Amazonka.CodeArtifact.Types.RepositorySummary
import Amazonka.CodeArtifact.Types.ResourcePolicy
import Amazonka.CodeArtifact.Types.SuccessfulPackageVersionInfo
import Amazonka.CodeArtifact.Types.Tag
import Amazonka.CodeArtifact.Types.UpstreamRepository
import Amazonka.CodeArtifact.Types.UpstreamRepositoryInfo
import Amazonka.CodeArtifact.UntagResource
import Amazonka.CodeArtifact.UpdatePackageVersionsStatus
import Amazonka.CodeArtifact.UpdateRepository
