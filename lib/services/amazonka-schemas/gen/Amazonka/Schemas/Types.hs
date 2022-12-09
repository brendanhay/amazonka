{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Schemas.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _GoneException,
    _InternalServerErrorException,
    _NotFoundException,
    _PreconditionFailedException,
    _ServiceUnavailableException,
    _TooManyRequestsException,
    _UnauthorizedException,

    -- * CodeGenerationStatus
    CodeGenerationStatus (..),

    -- * DiscovererState
    DiscovererState (..),

    -- * Type
    Type (..),

    -- * DiscovererSummary
    DiscovererSummary (..),
    newDiscovererSummary,
    discovererSummary_crossAccount,
    discovererSummary_discovererArn,
    discovererSummary_discovererId,
    discovererSummary_sourceArn,
    discovererSummary_state,
    discovererSummary_tags,

    -- * RegistrySummary
    RegistrySummary (..),
    newRegistrySummary,
    registrySummary_registryArn,
    registrySummary_registryName,
    registrySummary_tags,

    -- * SchemaSummary
    SchemaSummary (..),
    newSchemaSummary,
    schemaSummary_lastModified,
    schemaSummary_schemaArn,
    schemaSummary_schemaName,
    schemaSummary_tags,
    schemaSummary_versionCount,

    -- * SchemaVersionSummary
    SchemaVersionSummary (..),
    newSchemaVersionSummary,
    schemaVersionSummary_schemaArn,
    schemaVersionSummary_schemaName,
    schemaVersionSummary_schemaVersion,
    schemaVersionSummary_type,

    -- * SearchSchemaSummary
    SearchSchemaSummary (..),
    newSearchSchemaSummary,
    searchSchemaSummary_registryName,
    searchSchemaSummary_schemaArn,
    searchSchemaSummary_schemaName,
    searchSchemaSummary_schemaVersions,

    -- * SearchSchemaVersionSummary
    SearchSchemaVersionSummary (..),
    newSearchSchemaVersionSummary,
    searchSchemaVersionSummary_createdDate,
    searchSchemaVersionSummary_schemaVersion,
    searchSchemaVersionSummary_type,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Schemas.Types.CodeGenerationStatus
import Amazonka.Schemas.Types.DiscovererState
import Amazonka.Schemas.Types.DiscovererSummary
import Amazonka.Schemas.Types.RegistrySummary
import Amazonka.Schemas.Types.SchemaSummary
import Amazonka.Schemas.Types.SchemaVersionSummary
import Amazonka.Schemas.Types.SearchSchemaSummary
import Amazonka.Schemas.Types.SearchSchemaVersionSummary
import Amazonka.Schemas.Types.Type
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-02@ of the Amazon Schemas SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Schemas",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "schemas",
      Core.signingName = "schemas",
      Core.version = "2019-12-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Schemas",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Prism for BadRequestException' errors.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Prism for ConflictException' errors.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Prism for ForbiddenException' errors.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Prism for GoneException' errors.
_GoneException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GoneException =
  Core._MatchServiceError
    defaultService
    "GoneException"
    Prelude.. Core.hasStatus 410

-- | Prism for InternalServerErrorException' errors.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Prism for NotFoundException' errors.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Prism for PreconditionFailedException' errors.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | Prism for ServiceUnavailableException' errors.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Prism for TooManyRequestsException' errors.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | Prism for UnauthorizedException' errors.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401
