{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- 
module Network.AWS.KinesisVideoArchivedMedia
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidMediaFrameException
    , _InvalidMediaFrameException

    -- ** NoDataRetentionException
    , _NoDataRetentionException

    -- ** InvalidArgumentException
    , _InvalidArgumentException

    -- ** NotAuthorizedException
    , _NotAuthorizedException

    -- ** ClientLimitExceededException
    , _ClientLimitExceededException

    -- ** UnsupportedStreamMediaTypeException
    , _UnsupportedStreamMediaTypeException

    -- ** InvalidCodecPrivateDataException
    , _InvalidCodecPrivateDataException

    -- ** MissingCodecPrivateDataException
    , _MissingCodecPrivateDataException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetHLSStreamingSessionURL 
    , module Network.AWS.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL

    -- ** GetClip 
    , module Network.AWS.KinesisVideoArchivedMedia.GetClip

    -- ** GetMediaForFragmentList 
    , module Network.AWS.KinesisVideoArchivedMedia.GetMediaForFragmentList

    -- ** ListFragments (Paginated)
    , module Network.AWS.KinesisVideoArchivedMedia.ListFragments

    -- ** GetDASHStreamingSessionURL 
    , module Network.AWS.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL

    -- * Types

    -- ** DASHStreamingSessionURL
    , DASHStreamingSessionURL (..)

    -- ** HLSDiscontinuityMode
    , HLSDiscontinuityMode (..)

    -- ** DASHDisplayFragmentTimestamp
    , DASHDisplayFragmentTimestamp (..)

    -- ** HLSFragmentSelector
    , HLSFragmentSelector (..)
    , mkHLSFragmentSelector
    , hlsfsFragmentSelectorType
    , hlsfsTimestampRange

    -- ** FragmentSelector
    , FragmentSelector (..)
    , mkFragmentSelector
    , fsFragmentSelectorType
    , fsTimestampRange

    -- ** Fragment
    , Fragment (..)
    , mkFragment
    , fFragmentLengthInMilliseconds
    , fFragmentNumber
    , fFragmentSizeInBytes
    , fProducerTimestamp
    , fServerTimestamp

    -- ** HLSDisplayFragmentTimestamp
    , HLSDisplayFragmentTimestamp (..)

    -- ** DASHFragmentSelector
    , DASHFragmentSelector (..)
    , mkDASHFragmentSelector
    , dashfsFragmentSelectorType
    , dashfsTimestampRange

    -- ** ClipFragmentSelector
    , ClipFragmentSelector (..)
    , mkClipFragmentSelector
    , cfsFragmentSelectorType
    , cfsTimestampRange

    -- ** HLSTimestampRange
    , HLSTimestampRange (..)
    , mkHLSTimestampRange
    , hlstrEndTimestamp
    , hlstrStartTimestamp

    -- ** FragmentNumberString
    , FragmentNumberString (..)

    -- ** HLSStreamingSessionURL
    , HLSStreamingSessionURL (..)

    -- ** NextToken
    , NextToken (..)

    -- ** ContainerFormat
    , ContainerFormat (..)

    -- ** ResourceARN
    , ResourceARN (..)

    -- ** DASHTimestampRange
    , DASHTimestampRange (..)
    , mkDASHTimestampRange
    , dashtrEndTimestamp
    , dashtrStartTimestamp

    -- ** ClipTimestampRange
    , ClipTimestampRange (..)
    , mkClipTimestampRange
    , ctrStartTimestamp
    , ctrEndTimestamp

    -- ** DASHDisplayFragmentNumber
    , DASHDisplayFragmentNumber (..)

    -- ** HLSFragmentSelectorType
    , HLSFragmentSelectorType (..)

    -- ** DASHPlaybackMode
    , DASHPlaybackMode (..)

    -- ** FragmentSelectorType
    , FragmentSelectorType (..)

    -- ** DASHFragmentSelectorType
    , DASHFragmentSelectorType (..)

    -- ** HLSPlaybackMode
    , HLSPlaybackMode (..)

    -- ** StreamName
    , StreamName (..)

    -- ** ClipFragmentSelectorType
    , ClipFragmentSelectorType (..)

    -- ** ContentType
    , ContentType (..)

    -- ** TimestampRange
    , TimestampRange (..)
    , mkTimestampRange
    , trStartTimestamp
    , trEndTimestamp

    -- ** FragmentNumber
    , FragmentNumber (..)

    -- ** StreamARN
    , StreamARN (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.KinesisVideoArchivedMedia.Types
import Network.AWS.KinesisVideoArchivedMedia.Waiters
import Network.AWS.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
import Network.AWS.KinesisVideoArchivedMedia.GetClip
import Network.AWS.KinesisVideoArchivedMedia.GetMediaForFragmentList
import Network.AWS.KinesisVideoArchivedMedia.ListFragments
import Network.AWS.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'KinesisVideoArchivedMedia'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
