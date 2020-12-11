-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types
  ( -- * Service configuration
    kinesisVideoArchivedMediaService,

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
    ClipFragmentSelector (..),
    mkClipFragmentSelector,
    cfsFragmentSelectorType,
    cfsTimestampRange,

    -- * ClipTimestampRange
    ClipTimestampRange (..),
    mkClipTimestampRange,
    ctrStartTimestamp,
    ctrEndTimestamp,

    -- * DASHFragmentSelector
    DASHFragmentSelector (..),
    mkDASHFragmentSelector,
    dashfsFragmentSelectorType,
    dashfsTimestampRange,

    -- * DASHTimestampRange
    DASHTimestampRange (..),
    mkDASHTimestampRange,
    dashtrEndTimestamp,
    dashtrStartTimestamp,

    -- * Fragment
    Fragment (..),
    mkFragment,
    fFragmentLengthInMilliseconds,
    fServerTimestamp,
    fFragmentSizeInBytes,
    fFragmentNumber,
    fProducerTimestamp,

    -- * FragmentSelector
    FragmentSelector (..),
    mkFragmentSelector,
    fsFragmentSelectorType,
    fsTimestampRange,

    -- * HLSFragmentSelector
    HLSFragmentSelector (..),
    mkHLSFragmentSelector,
    hlsfsFragmentSelectorType,
    hlsfsTimestampRange,

    -- * HLSTimestampRange
    HLSTimestampRange (..),
    mkHLSTimestampRange,
    hlstrEndTimestamp,
    hlstrStartTimestamp,

    -- * TimestampRange
    TimestampRange (..),
    mkTimestampRange,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Archived Media SDK configuration.
kinesisVideoArchivedMediaService :: Lude.Service
kinesisVideoArchivedMediaService =
  Lude.Service
    { Lude._svcAbbrev = "KinesisVideoArchivedMedia",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "kinesisvideo",
      Lude._svcVersion = "2017-09-30",
      Lude._svcEndpoint =
        Lude.defaultEndpoint kinesisVideoArchivedMediaService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "KinesisVideoArchivedMedia",
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
