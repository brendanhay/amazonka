{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeArtifact.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Lens
  ( -- * Operations

    -- ** DisposePackageVersions
    disposePackageVersions_expectedStatus,
    disposePackageVersions_versionRevisions,
    disposePackageVersions_namespace,
    disposePackageVersions_domainOwner,
    disposePackageVersions_domain,
    disposePackageVersions_repository,
    disposePackageVersions_format,
    disposePackageVersions_package,
    disposePackageVersions_versions,
    disposePackageVersionsResponse_failedVersions,
    disposePackageVersionsResponse_successfulVersions,
    disposePackageVersionsResponse_httpStatus,

    -- ** GetRepositoryEndpoint
    getRepositoryEndpoint_domainOwner,
    getRepositoryEndpoint_domain,
    getRepositoryEndpoint_repository,
    getRepositoryEndpoint_format,
    getRepositoryEndpointResponse_repositoryEndpoint,
    getRepositoryEndpointResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListPackageVersionDependencies
    listPackageVersionDependencies_namespace,
    listPackageVersionDependencies_domainOwner,
    listPackageVersionDependencies_nextToken,
    listPackageVersionDependencies_domain,
    listPackageVersionDependencies_repository,
    listPackageVersionDependencies_format,
    listPackageVersionDependencies_package,
    listPackageVersionDependencies_packageVersion,
    listPackageVersionDependenciesResponse_format,
    listPackageVersionDependenciesResponse_namespace,
    listPackageVersionDependenciesResponse_versionRevision,
    listPackageVersionDependenciesResponse_dependencies,
    listPackageVersionDependenciesResponse_nextToken,
    listPackageVersionDependenciesResponse_version,
    listPackageVersionDependenciesResponse_package,
    listPackageVersionDependenciesResponse_httpStatus,

    -- ** ListPackages
    listPackages_format,
    listPackages_namespace,
    listPackages_domainOwner,
    listPackages_nextToken,
    listPackages_packagePrefix,
    listPackages_maxResults,
    listPackages_domain,
    listPackages_repository,
    listPackagesResponse_packages,
    listPackagesResponse_nextToken,
    listPackagesResponse_httpStatus,

    -- ** PutRepositoryPermissionsPolicy
    putRepositoryPermissionsPolicy_policyRevision,
    putRepositoryPermissionsPolicy_domainOwner,
    putRepositoryPermissionsPolicy_domain,
    putRepositoryPermissionsPolicy_repository,
    putRepositoryPermissionsPolicy_policyDocument,
    putRepositoryPermissionsPolicyResponse_policy,
    putRepositoryPermissionsPolicyResponse_httpStatus,

    -- ** DeleteRepositoryPermissionsPolicy
    deleteRepositoryPermissionsPolicy_policyRevision,
    deleteRepositoryPermissionsPolicy_domainOwner,
    deleteRepositoryPermissionsPolicy_domain,
    deleteRepositoryPermissionsPolicy_repository,
    deleteRepositoryPermissionsPolicyResponse_policy,
    deleteRepositoryPermissionsPolicyResponse_httpStatus,

    -- ** GetDomainPermissionsPolicy
    getDomainPermissionsPolicy_domainOwner,
    getDomainPermissionsPolicy_domain,
    getDomainPermissionsPolicyResponse_policy,
    getDomainPermissionsPolicyResponse_httpStatus,

    -- ** ListRepositories
    listRepositories_repositoryPrefix,
    listRepositories_nextToken,
    listRepositories_maxResults,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_httpStatus,

    -- ** UpdatePackageVersionsStatus
    updatePackageVersionsStatus_expectedStatus,
    updatePackageVersionsStatus_versionRevisions,
    updatePackageVersionsStatus_namespace,
    updatePackageVersionsStatus_domainOwner,
    updatePackageVersionsStatus_domain,
    updatePackageVersionsStatus_repository,
    updatePackageVersionsStatus_format,
    updatePackageVersionsStatus_package,
    updatePackageVersionsStatus_versions,
    updatePackageVersionsStatus_targetStatus,
    updatePackageVersionsStatusResponse_failedVersions,
    updatePackageVersionsStatusResponse_successfulVersions,
    updatePackageVersionsStatusResponse_httpStatus,

    -- ** CreateRepository
    createRepository_upstreams,
    createRepository_domainOwner,
    createRepository_description,
    createRepository_tags,
    createRepository_domain,
    createRepository_repository,
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,

    -- ** GetPackageVersionAsset
    getPackageVersionAsset_namespace,
    getPackageVersionAsset_domainOwner,
    getPackageVersionAsset_packageVersionRevision,
    getPackageVersionAsset_domain,
    getPackageVersionAsset_repository,
    getPackageVersionAsset_format,
    getPackageVersionAsset_package,
    getPackageVersionAsset_packageVersion,
    getPackageVersionAsset_asset,
    getPackageVersionAssetResponse_packageVersion,
    getPackageVersionAssetResponse_assetName,
    getPackageVersionAssetResponse_packageVersionRevision,
    getPackageVersionAssetResponse_httpStatus,
    getPackageVersionAssetResponse_asset,

    -- ** ListRepositoriesInDomain
    listRepositoriesInDomain_repositoryPrefix,
    listRepositoriesInDomain_domainOwner,
    listRepositoriesInDomain_nextToken,
    listRepositoriesInDomain_administratorAccount,
    listRepositoriesInDomain_maxResults,
    listRepositoriesInDomain_domain,
    listRepositoriesInDomainResponse_repositories,
    listRepositoriesInDomainResponse_nextToken,
    listRepositoriesInDomainResponse_httpStatus,

    -- ** PutDomainPermissionsPolicy
    putDomainPermissionsPolicy_policyRevision,
    putDomainPermissionsPolicy_domainOwner,
    putDomainPermissionsPolicy_domain,
    putDomainPermissionsPolicy_policyDocument,
    putDomainPermissionsPolicyResponse_policy,
    putDomainPermissionsPolicyResponse_httpStatus,

    -- ** DeleteDomainPermissionsPolicy
    deleteDomainPermissionsPolicy_policyRevision,
    deleteDomainPermissionsPolicy_domainOwner,
    deleteDomainPermissionsPolicy_domain,
    deleteDomainPermissionsPolicyResponse_policy,
    deleteDomainPermissionsPolicyResponse_httpStatus,

    -- ** GetPackageVersionReadme
    getPackageVersionReadme_namespace,
    getPackageVersionReadme_domainOwner,
    getPackageVersionReadme_domain,
    getPackageVersionReadme_repository,
    getPackageVersionReadme_format,
    getPackageVersionReadme_package,
    getPackageVersionReadme_packageVersion,
    getPackageVersionReadmeResponse_readme,
    getPackageVersionReadmeResponse_format,
    getPackageVersionReadmeResponse_namespace,
    getPackageVersionReadmeResponse_versionRevision,
    getPackageVersionReadmeResponse_version,
    getPackageVersionReadmeResponse_package,
    getPackageVersionReadmeResponse_httpStatus,

    -- ** CreateDomain
    createDomain_encryptionKey,
    createDomain_tags,
    createDomain_domain,
    createDomainResponse_domain,
    createDomainResponse_httpStatus,

    -- ** GetRepositoryPermissionsPolicy
    getRepositoryPermissionsPolicy_domainOwner,
    getRepositoryPermissionsPolicy_domain,
    getRepositoryPermissionsPolicy_repository,
    getRepositoryPermissionsPolicyResponse_policy,
    getRepositoryPermissionsPolicyResponse_httpStatus,

    -- ** AssociateExternalConnection
    associateExternalConnection_domainOwner,
    associateExternalConnection_domain,
    associateExternalConnection_repository,
    associateExternalConnection_externalConnection,
    associateExternalConnectionResponse_repository,
    associateExternalConnectionResponse_httpStatus,

    -- ** DescribeRepository
    describeRepository_domainOwner,
    describeRepository_domain,
    describeRepository_repository,
    describeRepositoryResponse_repository,
    describeRepositoryResponse_httpStatus,

    -- ** DeletePackageVersions
    deletePackageVersions_expectedStatus,
    deletePackageVersions_namespace,
    deletePackageVersions_domainOwner,
    deletePackageVersions_domain,
    deletePackageVersions_repository,
    deletePackageVersions_format,
    deletePackageVersions_package,
    deletePackageVersions_versions,
    deletePackageVersionsResponse_failedVersions,
    deletePackageVersionsResponse_successfulVersions,
    deletePackageVersionsResponse_httpStatus,

    -- ** DescribeDomain
    describeDomain_domainOwner,
    describeDomain_domain,
    describeDomainResponse_domain,
    describeDomainResponse_httpStatus,

    -- ** DescribePackageVersion
    describePackageVersion_namespace,
    describePackageVersion_domainOwner,
    describePackageVersion_domain,
    describePackageVersion_repository,
    describePackageVersion_format,
    describePackageVersion_package,
    describePackageVersion_packageVersion,
    describePackageVersionResponse_httpStatus,
    describePackageVersionResponse_packageVersion,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_domainOwner,
    deleteRepository_domain,
    deleteRepository_repository,
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,

    -- ** UpdateRepository
    updateRepository_upstreams,
    updateRepository_domainOwner,
    updateRepository_description,
    updateRepository_domain,
    updateRepository_repository,
    updateRepositoryResponse_repository,
    updateRepositoryResponse_httpStatus,

    -- ** CopyPackageVersions
    copyPackageVersions_allowOverwrite,
    copyPackageVersions_versionRevisions,
    copyPackageVersions_namespace,
    copyPackageVersions_domainOwner,
    copyPackageVersions_versions,
    copyPackageVersions_includeFromUpstream,
    copyPackageVersions_domain,
    copyPackageVersions_sourceRepository,
    copyPackageVersions_destinationRepository,
    copyPackageVersions_format,
    copyPackageVersions_package,
    copyPackageVersionsResponse_failedVersions,
    copyPackageVersionsResponse_successfulVersions,
    copyPackageVersionsResponse_httpStatus,

    -- ** GetAuthorizationToken
    getAuthorizationToken_domainOwner,
    getAuthorizationToken_durationSeconds,
    getAuthorizationToken_domain,
    getAuthorizationTokenResponse_expiration,
    getAuthorizationTokenResponse_authorizationToken,
    getAuthorizationTokenResponse_httpStatus,

    -- ** DisassociateExternalConnection
    disassociateExternalConnection_domainOwner,
    disassociateExternalConnection_domain,
    disassociateExternalConnection_repository,
    disassociateExternalConnection_externalConnection,
    disassociateExternalConnectionResponse_repository,
    disassociateExternalConnectionResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainOwner,
    deleteDomain_domain,
    deleteDomainResponse_domain,
    deleteDomainResponse_httpStatus,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_nextToken,
    listDomainsResponse_domains,
    listDomainsResponse_httpStatus,

    -- ** ListPackageVersions
    listPackageVersions_status,
    listPackageVersions_namespace,
    listPackageVersions_domainOwner,
    listPackageVersions_nextToken,
    listPackageVersions_maxResults,
    listPackageVersions_sortBy,
    listPackageVersions_domain,
    listPackageVersions_repository,
    listPackageVersions_format,
    listPackageVersions_package,
    listPackageVersionsResponse_format,
    listPackageVersionsResponse_namespace,
    listPackageVersionsResponse_versions,
    listPackageVersionsResponse_nextToken,
    listPackageVersionsResponse_package,
    listPackageVersionsResponse_defaultDisplayVersion,
    listPackageVersionsResponse_httpStatus,

    -- ** ListPackageVersionAssets
    listPackageVersionAssets_namespace,
    listPackageVersionAssets_domainOwner,
    listPackageVersionAssets_nextToken,
    listPackageVersionAssets_maxResults,
    listPackageVersionAssets_domain,
    listPackageVersionAssets_repository,
    listPackageVersionAssets_format,
    listPackageVersionAssets_package,
    listPackageVersionAssets_packageVersion,
    listPackageVersionAssetsResponse_format,
    listPackageVersionAssetsResponse_namespace,
    listPackageVersionAssetsResponse_versionRevision,
    listPackageVersionAssetsResponse_nextToken,
    listPackageVersionAssetsResponse_version,
    listPackageVersionAssetsResponse_package,
    listPackageVersionAssetsResponse_assets,
    listPackageVersionAssetsResponse_httpStatus,

    -- * Types

    -- ** AssetSummary
    assetSummary_size,
    assetSummary_hashes,
    assetSummary_name,

    -- ** DomainDescription
    domainDescription_assetSizeBytes,
    domainDescription_status,
    domainDescription_arn,
    domainDescription_createdTime,
    domainDescription_s3BucketArn,
    domainDescription_owner,
    domainDescription_repositoryCount,
    domainDescription_name,
    domainDescription_encryptionKey,

    -- ** DomainSummary
    domainSummary_status,
    domainSummary_arn,
    domainSummary_createdTime,
    domainSummary_owner,
    domainSummary_name,
    domainSummary_encryptionKey,

    -- ** LicenseInfo
    licenseInfo_url,
    licenseInfo_name,

    -- ** PackageDependency
    packageDependency_dependencyType,
    packageDependency_namespace,
    packageDependency_package,
    packageDependency_versionRequirement,

    -- ** PackageSummary
    packageSummary_format,
    packageSummary_namespace,
    packageSummary_package,

    -- ** PackageVersionDescription
    packageVersionDescription_summary,
    packageVersionDescription_homePage,
    packageVersionDescription_status,
    packageVersionDescription_sourceCodeRepository,
    packageVersionDescription_packageName,
    packageVersionDescription_publishedTime,
    packageVersionDescription_format,
    packageVersionDescription_namespace,
    packageVersionDescription_version,
    packageVersionDescription_displayName,
    packageVersionDescription_licenses,
    packageVersionDescription_revision,

    -- ** PackageVersionError
    packageVersionError_errorCode,
    packageVersionError_errorMessage,

    -- ** PackageVersionSummary
    packageVersionSummary_revision,
    packageVersionSummary_version,
    packageVersionSummary_status,

    -- ** RepositoryDescription
    repositoryDescription_upstreams,
    repositoryDescription_arn,
    repositoryDescription_domainOwner,
    repositoryDescription_domainName,
    repositoryDescription_name,
    repositoryDescription_administratorAccount,
    repositoryDescription_externalConnections,
    repositoryDescription_description,

    -- ** RepositoryExternalConnectionInfo
    repositoryExternalConnectionInfo_externalConnectionName,
    repositoryExternalConnectionInfo_status,
    repositoryExternalConnectionInfo_packageFormat,

    -- ** RepositorySummary
    repositorySummary_arn,
    repositorySummary_domainOwner,
    repositorySummary_domainName,
    repositorySummary_name,
    repositorySummary_administratorAccount,
    repositorySummary_description,

    -- ** ResourcePolicy
    resourcePolicy_resourceArn,
    resourcePolicy_document,
    resourcePolicy_revision,

    -- ** SuccessfulPackageVersionInfo
    successfulPackageVersionInfo_status,
    successfulPackageVersionInfo_revision,

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
import Amazonka.CodeArtifact.PutRepositoryPermissionsPolicy
import Amazonka.CodeArtifact.TagResource
import Amazonka.CodeArtifact.Types.AssetSummary
import Amazonka.CodeArtifact.Types.DomainDescription
import Amazonka.CodeArtifact.Types.DomainSummary
import Amazonka.CodeArtifact.Types.LicenseInfo
import Amazonka.CodeArtifact.Types.PackageDependency
import Amazonka.CodeArtifact.Types.PackageSummary
import Amazonka.CodeArtifact.Types.PackageVersionDescription
import Amazonka.CodeArtifact.Types.PackageVersionError
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
