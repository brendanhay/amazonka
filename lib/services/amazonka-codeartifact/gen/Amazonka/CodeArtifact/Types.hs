{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeArtifact.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * DomainStatus
    DomainStatus (..),

    -- * ExternalConnectionStatus
    ExternalConnectionStatus (..),

    -- * HashAlgorithm
    HashAlgorithm (..),

    -- * PackageFormat
    PackageFormat (..),

    -- * PackageVersionErrorCode
    PackageVersionErrorCode (..),

    -- * PackageVersionSortType
    PackageVersionSortType (..),

    -- * PackageVersionStatus
    PackageVersionStatus (..),

    -- * AssetSummary
    AssetSummary (..),
    newAssetSummary,
    assetSummary_size,
    assetSummary_hashes,
    assetSummary_name,

    -- * DomainDescription
    DomainDescription (..),
    newDomainDescription,
    domainDescription_assetSizeBytes,
    domainDescription_status,
    domainDescription_arn,
    domainDescription_createdTime,
    domainDescription_s3BucketArn,
    domainDescription_owner,
    domainDescription_repositoryCount,
    domainDescription_name,
    domainDescription_encryptionKey,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_status,
    domainSummary_arn,
    domainSummary_createdTime,
    domainSummary_owner,
    domainSummary_name,
    domainSummary_encryptionKey,

    -- * LicenseInfo
    LicenseInfo (..),
    newLicenseInfo,
    licenseInfo_url,
    licenseInfo_name,

    -- * PackageDependency
    PackageDependency (..),
    newPackageDependency,
    packageDependency_dependencyType,
    packageDependency_namespace,
    packageDependency_package,
    packageDependency_versionRequirement,

    -- * PackageSummary
    PackageSummary (..),
    newPackageSummary,
    packageSummary_format,
    packageSummary_namespace,
    packageSummary_package,

    -- * PackageVersionDescription
    PackageVersionDescription (..),
    newPackageVersionDescription,
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

    -- * PackageVersionError
    PackageVersionError (..),
    newPackageVersionError,
    packageVersionError_errorCode,
    packageVersionError_errorMessage,

    -- * PackageVersionSummary
    PackageVersionSummary (..),
    newPackageVersionSummary,
    packageVersionSummary_revision,
    packageVersionSummary_version,
    packageVersionSummary_status,

    -- * RepositoryDescription
    RepositoryDescription (..),
    newRepositoryDescription,
    repositoryDescription_upstreams,
    repositoryDescription_arn,
    repositoryDescription_domainOwner,
    repositoryDescription_domainName,
    repositoryDescription_name,
    repositoryDescription_administratorAccount,
    repositoryDescription_externalConnections,
    repositoryDescription_description,

    -- * RepositoryExternalConnectionInfo
    RepositoryExternalConnectionInfo (..),
    newRepositoryExternalConnectionInfo,
    repositoryExternalConnectionInfo_externalConnectionName,
    repositoryExternalConnectionInfo_status,
    repositoryExternalConnectionInfo_packageFormat,

    -- * RepositorySummary
    RepositorySummary (..),
    newRepositorySummary,
    repositorySummary_arn,
    repositorySummary_domainOwner,
    repositorySummary_domainName,
    repositorySummary_name,
    repositorySummary_administratorAccount,
    repositorySummary_description,

    -- * ResourcePolicy
    ResourcePolicy (..),
    newResourcePolicy,
    resourcePolicy_resourceArn,
    resourcePolicy_document,
    resourcePolicy_revision,

    -- * SuccessfulPackageVersionInfo
    SuccessfulPackageVersionInfo (..),
    newSuccessfulPackageVersionInfo,
    successfulPackageVersionInfo_status,
    successfulPackageVersionInfo_revision,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UpstreamRepository
    UpstreamRepository (..),
    newUpstreamRepository,
    upstreamRepository_repositoryName,

    -- * UpstreamRepositoryInfo
    UpstreamRepositoryInfo (..),
    newUpstreamRepositoryInfo,
    upstreamRepositoryInfo_repositoryName,
  )
where

import Amazonka.CodeArtifact.Types.AssetSummary
import Amazonka.CodeArtifact.Types.DomainDescription
import Amazonka.CodeArtifact.Types.DomainStatus
import Amazonka.CodeArtifact.Types.DomainSummary
import Amazonka.CodeArtifact.Types.ExternalConnectionStatus
import Amazonka.CodeArtifact.Types.HashAlgorithm
import Amazonka.CodeArtifact.Types.LicenseInfo
import Amazonka.CodeArtifact.Types.PackageDependency
import Amazonka.CodeArtifact.Types.PackageFormat
import Amazonka.CodeArtifact.Types.PackageSummary
import Amazonka.CodeArtifact.Types.PackageVersionDescription
import Amazonka.CodeArtifact.Types.PackageVersionError
import Amazonka.CodeArtifact.Types.PackageVersionErrorCode
import Amazonka.CodeArtifact.Types.PackageVersionSortType
import Amazonka.CodeArtifact.Types.PackageVersionStatus
import Amazonka.CodeArtifact.Types.PackageVersionSummary
import Amazonka.CodeArtifact.Types.RepositoryDescription
import Amazonka.CodeArtifact.Types.RepositoryExternalConnectionInfo
import Amazonka.CodeArtifact.Types.RepositorySummary
import Amazonka.CodeArtifact.Types.ResourcePolicy
import Amazonka.CodeArtifact.Types.SuccessfulPackageVersionInfo
import Amazonka.CodeArtifact.Types.Tag
import Amazonka.CodeArtifact.Types.UpstreamRepository
import Amazonka.CodeArtifact.Types.UpstreamRepositoryInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-09-22@ of the Amazon CodeArtifact SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CodeArtifact",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "codeartifact",
      Core._serviceSigningName = "codeartifact",
      Core._serviceVersion = "2018-09-22",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CodeArtifact",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The operation did not succeed because a parameter in the request was
-- sent with an invalid value.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | The operation did not succeed because of an unauthorized access attempt.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The operation did not succeed because prerequisites are not met.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The operation did not succeed because it would have exceeded a service
-- limit for your account.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The operation did not succeed because too many requests are sent to the
-- service.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The operation did not succeed because of an error that occurred inside
-- AWS CodeArtifact.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The operation did not succeed because the resource requested is not
-- found in the service.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
