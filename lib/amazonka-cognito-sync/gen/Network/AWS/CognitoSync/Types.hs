{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types
  ( -- * Service Configuration
    cognitoSync,

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
    CognitoStreams,
    cognitoStreams,
    csStreamingStatus,
    csStreamName,
    csRoleARN,

    -- * Dataset
    Dataset,
    dataset,
    dLastModifiedDate,
    dNumRecords,
    dDataStorage,
    dDatasetName,
    dCreationDate,
    dLastModifiedBy,
    dIdentityId,

    -- * IdentityPoolUsage
    IdentityPoolUsage,
    identityPoolUsage,
    ipuLastModifiedDate,
    ipuIdentityPoolId,
    ipuDataStorage,
    ipuSyncSessionsCount,

    -- * IdentityUsage
    IdentityUsage,
    identityUsage,
    iuLastModifiedDate,
    iuIdentityPoolId,
    iuDatasetCount,
    iuDataStorage,
    iuIdentityId,

    -- * PushSync
    PushSync,
    pushSync,
    psApplicationARNs,
    psRoleARN,

    -- * Record
    Record,
    record,
    rSyncCount,
    rDeviceLastModifiedDate,
    rLastModifiedDate,
    rValue,
    rKey,
    rLastModifiedBy,

    -- * RecordPatch
    RecordPatch,
    recordPatch,
    rpDeviceLastModifiedDate,
    rpValue,
    rpOp,
    rpKey,
    rpSyncCount,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-06-30@ of the Amazon Cognito Sync SDK configuration.
cognitoSync :: Service
cognitoSync =
  Service
    { _svcAbbrev = "CognitoSync",
      _svcSigner = v4,
      _svcPrefix = "cognito-sync",
      _svcVersion = "2014-06-30",
      _svcEndpoint = defaultEndpoint cognitoSync,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CognitoSync",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
