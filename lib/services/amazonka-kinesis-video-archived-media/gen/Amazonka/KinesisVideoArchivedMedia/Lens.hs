{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoArchivedMedia.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Lens
  ( -- * Operations

    -- ** GetClip
    getClip_streamARN,
    getClip_streamName,
    getClip_clipFragmentSelector,
    getClipResponse_contentType,
    getClipResponse_httpStatus,
    getClipResponse_payload,

    -- ** GetDASHStreamingSessionURL
    getDASHStreamingSessionURL_dASHFragmentSelector,
    getDASHStreamingSessionURL_displayFragmentNumber,
    getDASHStreamingSessionURL_displayFragmentTimestamp,
    getDASHStreamingSessionURL_expires,
    getDASHStreamingSessionURL_maxManifestFragmentResults,
    getDASHStreamingSessionURL_playbackMode,
    getDASHStreamingSessionURL_streamARN,
    getDASHStreamingSessionURL_streamName,
    getDASHStreamingSessionURLResponse_dASHStreamingSessionURL,
    getDASHStreamingSessionURLResponse_httpStatus,

    -- ** GetHLSStreamingSessionURL
    getHLSStreamingSessionURL_containerFormat,
    getHLSStreamingSessionURL_discontinuityMode,
    getHLSStreamingSessionURL_displayFragmentTimestamp,
    getHLSStreamingSessionURL_expires,
    getHLSStreamingSessionURL_hLSFragmentSelector,
    getHLSStreamingSessionURL_maxMediaPlaylistFragmentResults,
    getHLSStreamingSessionURL_playbackMode,
    getHLSStreamingSessionURL_streamARN,
    getHLSStreamingSessionURL_streamName,
    getHLSStreamingSessionURLResponse_hLSStreamingSessionURL,
    getHLSStreamingSessionURLResponse_httpStatus,

    -- ** GetImages
    getImages_formatConfig,
    getImages_heightPixels,
    getImages_maxResults,
    getImages_nextToken,
    getImages_streamARN,
    getImages_streamName,
    getImages_widthPixels,
    getImages_imageSelectorType,
    getImages_startTimestamp,
    getImages_endTimestamp,
    getImages_samplingInterval,
    getImages_format,
    getImagesResponse_images,
    getImagesResponse_nextToken,
    getImagesResponse_httpStatus,

    -- ** GetMediaForFragmentList
    getMediaForFragmentList_streamARN,
    getMediaForFragmentList_streamName,
    getMediaForFragmentList_fragments,
    getMediaForFragmentListResponse_contentType,
    getMediaForFragmentListResponse_httpStatus,
    getMediaForFragmentListResponse_payload,

    -- ** ListFragments
    listFragments_fragmentSelector,
    listFragments_maxResults,
    listFragments_nextToken,
    listFragments_streamARN,
    listFragments_streamName,
    listFragmentsResponse_fragments,
    listFragmentsResponse_nextToken,
    listFragmentsResponse_httpStatus,

    -- * Types

    -- ** ClipFragmentSelector
    clipFragmentSelector_fragmentSelectorType,
    clipFragmentSelector_timestampRange,

    -- ** ClipTimestampRange
    clipTimestampRange_startTimestamp,
    clipTimestampRange_endTimestamp,

    -- ** DASHFragmentSelector
    dASHFragmentSelector_fragmentSelectorType,
    dASHFragmentSelector_timestampRange,

    -- ** DASHTimestampRange
    dASHTimestampRange_endTimestamp,
    dASHTimestampRange_startTimestamp,

    -- ** Fragment
    fragment_fragmentLengthInMilliseconds,
    fragment_fragmentNumber,
    fragment_fragmentSizeInBytes,
    fragment_producerTimestamp,
    fragment_serverTimestamp,

    -- ** FragmentSelector
    fragmentSelector_fragmentSelectorType,
    fragmentSelector_timestampRange,

    -- ** HLSFragmentSelector
    hLSFragmentSelector_fragmentSelectorType,
    hLSFragmentSelector_timestampRange,

    -- ** HLSTimestampRange
    hLSTimestampRange_endTimestamp,
    hLSTimestampRange_startTimestamp,

    -- ** Image
    image_error,
    image_imageContent,
    image_timeStamp,

    -- ** TimestampRange
    timestampRange_startTimestamp,
    timestampRange_endTimestamp,
  )
where

import Amazonka.KinesisVideoArchivedMedia.GetClip
import Amazonka.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
import Amazonka.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
import Amazonka.KinesisVideoArchivedMedia.GetImages
import Amazonka.KinesisVideoArchivedMedia.GetMediaForFragmentList
import Amazonka.KinesisVideoArchivedMedia.ListFragments
import Amazonka.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import Amazonka.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.DASHTimestampRange
import Amazonka.KinesisVideoArchivedMedia.Types.Fragment
import Amazonka.KinesisVideoArchivedMedia.Types.FragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
import Amazonka.KinesisVideoArchivedMedia.Types.HLSTimestampRange
import Amazonka.KinesisVideoArchivedMedia.Types.Image
import Amazonka.KinesisVideoArchivedMedia.Types.TimestampRange
