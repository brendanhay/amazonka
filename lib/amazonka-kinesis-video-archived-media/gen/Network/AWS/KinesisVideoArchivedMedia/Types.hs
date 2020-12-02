{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types
  ( -- * Service Configuration
    kinesisVideoArchivedMedia,

    -- * Errors

    -- * ClipFragmentSelectorType
    ClipFragmentSelectorType (..),

    -- * ContainerFormat
    ContainerFormat (..),

    -- * DASHDisplayFragmentNumber
    DASHDisplayFragmentNumber (..),

    -- * DASHDisplayFragmentTimestamp
    DASHDisplayFragmentTimestamp (..),

    -- * DASHFragmentSelectorType
    DASHFragmentSelectorType (..),

    -- * DASHPlaybackMode
    DASHPlaybackMode (..),

    -- * FragmentSelectorType
    FragmentSelectorType (..),

    -- * HLSDiscontinuityMode
    HLSDiscontinuityMode (..),

    -- * HLSDisplayFragmentTimestamp
    HLSDisplayFragmentTimestamp (..),

    -- * HLSFragmentSelectorType
    HLSFragmentSelectorType (..),

    -- * HLSPlaybackMode
    HLSPlaybackMode (..),

    -- * ClipFragmentSelector
    ClipFragmentSelector,
    clipFragmentSelector,
    cfsFragmentSelectorType,
    cfsTimestampRange,

    -- * ClipTimestampRange
    ClipTimestampRange,
    clipTimestampRange,
    ctrStartTimestamp,
    ctrEndTimestamp,

    -- * DASHFragmentSelector
    DASHFragmentSelector,
    dASHFragmentSelector,
    dashfsFragmentSelectorType,
    dashfsTimestampRange,

    -- * DASHTimestampRange
    DASHTimestampRange,
    dASHTimestampRange,
    dashtrEndTimestamp,
    dashtrStartTimestamp,

    -- * Fragment
    Fragment,
    fragment,
    fFragmentLengthInMilliseconds,
    fServerTimestamp,
    fFragmentSizeInBytes,
    fFragmentNumber,
    fProducerTimestamp,

    -- * FragmentSelector
    FragmentSelector,
    fragmentSelector,
    fsFragmentSelectorType,
    fsTimestampRange,

    -- * HLSFragmentSelector
    HLSFragmentSelector,
    hLSFragmentSelector,
    hlsfsFragmentSelectorType,
    hlsfsTimestampRange,

    -- * HLSTimestampRange
    HLSTimestampRange,
    hLSTimestampRange,
    hlstrEndTimestamp,
    hlstrStartTimestamp,

    -- * TimestampRange
    TimestampRange,
    timestampRange,
    trStartTimestamp,
    trEndTimestamp,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.Fragment
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Archived Media SDK configuration.
kinesisVideoArchivedMedia :: Service
kinesisVideoArchivedMedia =
  Service
    { _svcAbbrev = "KinesisVideoArchivedMedia",
      _svcSigner = v4,
      _svcPrefix = "kinesisvideo",
      _svcVersion = "2017-09-30",
      _svcEndpoint = defaultEndpoint kinesisVideoArchivedMedia,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "KinesisVideoArchivedMedia",
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
