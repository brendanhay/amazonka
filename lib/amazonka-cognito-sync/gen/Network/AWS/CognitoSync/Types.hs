-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types
  ( -- * Service configuration
    cognitoSyncService,

    -- * Errors

    -- * BulkPublishStatus
    BulkPublishStatus (..),

    -- * Operation
    Operation (..),

    -- * Platform
    Platform (..),

    -- * StreamingStatus
    StreamingStatus (..),

    -- * CognitoStreams
    CognitoStreams (..),
    mkCognitoStreams,
    csStreamingStatus,
    csStreamName,
    csRoleARN,

    -- * Dataset
    Dataset (..),
    mkDataset,
    dLastModifiedDate,
    dNumRecords,
    dDataStorage,
    dDatasetName,
    dCreationDate,
    dLastModifiedBy,
    dIdentityId,

    -- * IdentityPoolUsage
    IdentityPoolUsage (..),
    mkIdentityPoolUsage,
    ipuLastModifiedDate,
    ipuIdentityPoolId,
    ipuDataStorage,
    ipuSyncSessionsCount,

    -- * IdentityUsage
    IdentityUsage (..),
    mkIdentityUsage,
    iuLastModifiedDate,
    iuIdentityPoolId,
    iuDatasetCount,
    iuDataStorage,
    iuIdentityId,

    -- * PushSync
    PushSync (..),
    mkPushSync,
    psApplicationARNs,
    psRoleARN,

    -- * Record
    Record (..),
    mkRecord,
    rSyncCount,
    rDeviceLastModifiedDate,
    rLastModifiedDate,
    rValue,
    rKey,
    rLastModifiedBy,

    -- * RecordPatch
    RecordPatch (..),
    mkRecordPatch,
    rpSyncCount,
    rpDeviceLastModifiedDate,
    rpOp,
    rpValue,
    rpKey,
  )
where

import Network.AWS.CognitoSync.Types.BulkPublishStatus
import Network.AWS.CognitoSync.Types.CognitoStreams
import Network.AWS.CognitoSync.Types.Dataset
import Network.AWS.CognitoSync.Types.IdentityPoolUsage
import Network.AWS.CognitoSync.Types.IdentityUsage
import Network.AWS.CognitoSync.Types.Operation
import Network.AWS.CognitoSync.Types.Platform
import Network.AWS.CognitoSync.Types.PushSync
import Network.AWS.CognitoSync.Types.Record
import Network.AWS.CognitoSync.Types.RecordPatch
import Network.AWS.CognitoSync.Types.StreamingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Sync SDK configuration.
cognitoSyncService :: Lude.Service
cognitoSyncService =
  Lude.Service
    { Lude._svcAbbrev = "CognitoSync",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cognito-sync",
      Lude._svcVersion = "2014-06-30",
      Lude._svcEndpoint = Lude.defaultEndpoint cognitoSyncService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CognitoSync",
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
