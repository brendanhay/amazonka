{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeStarConnections.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarConnections.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _UnsupportedOperationException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ConflictException,

    -- * ConnectionStatus
    ConnectionStatus (..),

    -- * ProviderType
    ProviderType (..),

    -- * Connection
    Connection (..),
    newConnection,
    connection_hostArn,
    connection_connectionArn,
    connection_providerType,
    connection_ownerAccountId,
    connection_connectionStatus,
    connection_connectionName,

    -- * Host
    Host (..),
    newHost,
    host_vpcConfiguration,
    host_name,
    host_hostArn,
    host_providerEndpoint,
    host_status,
    host_providerType,
    host_statusMessage,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * VpcConfiguration
    VpcConfiguration (..),
    newVpcConfiguration,
    vpcConfiguration_tlsCertificate,
    vpcConfiguration_vpcId,
    vpcConfiguration_subnetIds,
    vpcConfiguration_securityGroupIds,
  )
where

import Amazonka.CodeStarConnections.Types.Connection
import Amazonka.CodeStarConnections.Types.ConnectionStatus
import Amazonka.CodeStarConnections.Types.Host
import Amazonka.CodeStarConnections.Types.ProviderType
import Amazonka.CodeStarConnections.Types.Tag
import Amazonka.CodeStarConnections.Types.VpcConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-01@ of the Amazon CodeStar connections SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CodeStarConnections",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "codestar-connections",
      Core.signingName = "codestar-connections",
      Core.version = "2019-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "CodeStarConnections",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Resource not found. Verify the ARN for the host resource and try again.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The operation is not supported. Check the connection status and try
-- again.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | Resource not found. Verify the connection resource ARN and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Exceeded the maximum limit for connections.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Two conflicting operations have been made on the same resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
