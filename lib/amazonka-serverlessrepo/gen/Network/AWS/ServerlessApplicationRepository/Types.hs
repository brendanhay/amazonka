-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types
  ( -- * Service configuration
    serverlessApplicationRepositoryService,

    -- * Errors

    -- * Capability
    Capability (..),

    -- * Status
    Status (..),

    -- * ApplicationDependencySummary
    ApplicationDependencySummary (..),
    mkApplicationDependencySummary,
    adsApplicationId,
    adsSemanticVersion,

    -- * ApplicationPolicyStatement
    ApplicationPolicyStatement (..),
    mkApplicationPolicyStatement,
    apsStatementId,
    apsPrincipalOrgIds,
    apsPrincipals,
    apsActions,

    -- * ApplicationSummary
    ApplicationSummary (..),
    mkApplicationSummary,
    asCreationTime,
    asHomePageURL,
    asLabels,
    asSpdxLicenseId,
    asDescription,
    asAuthor,
    asApplicationId,
    asName,

    -- * ParameterDefinition
    ParameterDefinition (..),
    mkParameterDefinition,
    pdMaxValue,
    pdMaxLength,
    pdConstraintDescription,
    pdMinLength,
    pdDefaultValue,
    pdAllowedPattern,
    pdNoEcho,
    pdType,
    pdAllowedValues,
    pdDescription,
    pdMinValue,
    pdReferencedByResources,
    pdName,

    -- * ParameterValue
    ParameterValue (..),
    mkParameterValue,
    pvValue,
    pvName,

    -- * RollbackConfiguration
    RollbackConfiguration (..),
    mkRollbackConfiguration,
    rcRollbackTriggers,
    rcMonitoringTimeInMinutes,

    -- * RollbackTrigger
    RollbackTrigger (..),
    mkRollbackTrigger,
    rtType,
    rtARN,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Version
    Version (..),
    mkVersion,
    vSourceCodeURL,
    vSourceCodeArchiveURL,
    vTemplateURL,
    vParameterDefinitions,
    vResourcesSupported,
    vCreationTime,
    vRequiredCapabilities,
    vApplicationId,
    vSemanticVersion,

    -- * VersionSummary
    VersionSummary (..),
    mkVersionSummary,
    vsSourceCodeURL,
    vsCreationTime,
    vsApplicationId,
    vsSemanticVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
import Network.AWS.ServerlessApplicationRepository.Types.Capability
import Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
import Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
import Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
import Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
import Network.AWS.ServerlessApplicationRepository.Types.Status
import Network.AWS.ServerlessApplicationRepository.Types.Tag
import Network.AWS.ServerlessApplicationRepository.Types.Version
import Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-08@ of the Amazon ServerlessApplicationRepository SDK configuration.
serverlessApplicationRepositoryService :: Lude.Service
serverlessApplicationRepositoryService =
  Lude.Service
    { Lude._svcAbbrev = "ServerlessApplicationRepository",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "serverlessrepo",
      Lude._svcVersion = "2017-09-08",
      Lude._svcEndpoint =
        Lude.defaultEndpoint serverlessApplicationRepositoryService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError =
        Lude.parseJSONError "ServerlessApplicationRepository",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
