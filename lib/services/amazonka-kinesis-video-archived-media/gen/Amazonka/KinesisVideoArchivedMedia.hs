{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.KinesisVideoArchivedMedia
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-09-30@ of the AWS service descriptions, licensed under Apache 2.0.
module Amazonka.KinesisVideoArchivedMedia
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ClientLimitExceededException
    _ClientLimitExceededException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** InvalidCodecPrivateDataException
    _InvalidCodecPrivateDataException,

    -- ** InvalidMediaFrameException
    _InvalidMediaFrameException,

    -- ** MissingCodecPrivateDataException
    _MissingCodecPrivateDataException,

    -- ** NoDataRetentionException
    _NoDataRetentionException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** UnsupportedStreamMediaTypeException
    _UnsupportedStreamMediaTypeException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetClip
    GetClip (GetClip'),
    newGetClip,
    GetClipResponse (GetClipResponse'),
    newGetClipResponse,

    -- ** GetDASHStreamingSessionURL
    GetDASHStreamingSessionURL (GetDASHStreamingSessionURL'),
    newGetDASHStreamingSessionURL,
    GetDASHStreamingSessionURLResponse (GetDASHStreamingSessionURLResponse'),
    newGetDASHStreamingSessionURLResponse,

    -- ** GetHLSStreamingSessionURL
    GetHLSStreamingSessionURL (GetHLSStreamingSessionURL'),
    newGetHLSStreamingSessionURL,
    GetHLSStreamingSessionURLResponse (GetHLSStreamingSessionURLResponse'),
    newGetHLSStreamingSessionURLResponse,

    -- ** GetImages (Paginated)
    GetImages (GetImages'),
    newGetImages,
    GetImagesResponse (GetImagesResponse'),
    newGetImagesResponse,

    -- ** GetMediaForFragmentList
    GetMediaForFragmentList (GetMediaForFragmentList'),
    newGetMediaForFragmentList,
    GetMediaForFragmentListResponse (GetMediaForFragmentListResponse'),
    newGetMediaForFragmentListResponse,

    -- ** ListFragments (Paginated)
    ListFragments (ListFragments'),
    newListFragments,
    ListFragmentsResponse (ListFragmentsResponse'),
    newListFragmentsResponse,

    -- * Types

    -- ** ClipFragmentSelectorType
    ClipFragmentSelectorType (..),

    -- ** ContainerFormat
    ContainerFormat (..),

    -- ** DASHDisplayFragmentNumber
    DASHDisplayFragmentNumber (..),

    -- ** DASHDisplayFragmentTimestamp
    DASHDisplayFragmentTimestamp (..),

    -- ** DASHFragmentSelectorType
    DASHFragmentSelectorType (..),

    -- ** DASHPlaybackMode
    DASHPlaybackMode (..),

    -- ** Format
    Format (..),

    -- ** FormatConfigKey
    FormatConfigKey (..),

    -- ** FragmentSelectorType
    FragmentSelectorType (..),

    -- ** HLSDiscontinuityMode
    HLSDiscontinuityMode (..),

    -- ** HLSDisplayFragmentTimestamp
    HLSDisplayFragmentTimestamp (..),

    -- ** HLSFragmentSelectorType
    HLSFragmentSelectorType (..),

    -- ** HLSPlaybackMode
    HLSPlaybackMode (..),

    -- ** ImageError
    ImageError (..),

    -- ** ImageSelectorType
    ImageSelectorType (..),

    -- ** ClipFragmentSelector
    ClipFragmentSelector (ClipFragmentSelector'),
    newClipFragmentSelector,

    -- ** ClipTimestampRange
    ClipTimestampRange (ClipTimestampRange'),
    newClipTimestampRange,

    -- ** DASHFragmentSelector
    DASHFragmentSelector (DASHFragmentSelector'),
    newDASHFragmentSelector,

    -- ** DASHTimestampRange
    DASHTimestampRange (DASHTimestampRange'),
    newDASHTimestampRange,

    -- ** Fragment
    Fragment (Fragment'),
    newFragment,

    -- ** FragmentSelector
    FragmentSelector (FragmentSelector'),
    newFragmentSelector,

    -- ** HLSFragmentSelector
    HLSFragmentSelector (HLSFragmentSelector'),
    newHLSFragmentSelector,

    -- ** HLSTimestampRange
    HLSTimestampRange (HLSTimestampRange'),
    newHLSTimestampRange,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** TimestampRange
    TimestampRange (TimestampRange'),
    newTimestampRange,
  )
where

import Amazonka.KinesisVideoArchivedMedia.GetClip
import Amazonka.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
import Amazonka.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
import Amazonka.KinesisVideoArchivedMedia.GetImages
import Amazonka.KinesisVideoArchivedMedia.GetMediaForFragmentList
import Amazonka.KinesisVideoArchivedMedia.Lens
import Amazonka.KinesisVideoArchivedMedia.ListFragments
import Amazonka.KinesisVideoArchivedMedia.Types
import Amazonka.KinesisVideoArchivedMedia.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KinesisVideoArchivedMedia'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
