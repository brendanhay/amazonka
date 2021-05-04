{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Lens
  ( -- * Operations

    -- ** ListFragments
    listFragments_nextToken,
    listFragments_maxResults,
    listFragments_streamARN,
    listFragments_streamName,
    listFragments_fragmentSelector,
    listFragmentsResponse_nextToken,
    listFragmentsResponse_fragments,
    listFragmentsResponse_httpStatus,

    -- ** GetMediaForFragmentList
    getMediaForFragmentList_streamARN,
    getMediaForFragmentList_streamName,
    getMediaForFragmentList_fragments,
    getMediaForFragmentListResponse_contentType,
    getMediaForFragmentListResponse_httpStatus,
    getMediaForFragmentListResponse_payload,

    -- ** GetClip
    getClip_streamARN,
    getClip_streamName,
    getClip_clipFragmentSelector,
    getClipResponse_contentType,
    getClipResponse_httpStatus,
    getClipResponse_payload,

    -- ** GetDASHStreamingSessionURL
    getDASHStreamingSessionURL_displayFragmentTimestamp,
    getDASHStreamingSessionURL_displayFragmentNumber,
    getDASHStreamingSessionURL_maxManifestFragmentResults,
    getDASHStreamingSessionURL_dASHFragmentSelector,
    getDASHStreamingSessionURL_playbackMode,
    getDASHStreamingSessionURL_streamARN,
    getDASHStreamingSessionURL_streamName,
    getDASHStreamingSessionURL_expires,
    getDASHStreamingSessionURLResponse_dASHStreamingSessionURL,
    getDASHStreamingSessionURLResponse_httpStatus,

    -- ** GetHLSStreamingSessionURL
    getHLSStreamingSessionURL_maxMediaPlaylistFragmentResults,
    getHLSStreamingSessionURL_containerFormat,
    getHLSStreamingSessionURL_displayFragmentTimestamp,
    getHLSStreamingSessionURL_playbackMode,
    getHLSStreamingSessionURL_streamARN,
    getHLSStreamingSessionURL_streamName,
    getHLSStreamingSessionURL_expires,
    getHLSStreamingSessionURL_discontinuityMode,
    getHLSStreamingSessionURL_hLSFragmentSelector,
    getHLSStreamingSessionURLResponse_hLSStreamingSessionURL,
    getHLSStreamingSessionURLResponse_httpStatus,

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
    fragment_producerTimestamp,
    fragment_fragmentNumber,
    fragment_fragmentSizeInBytes,
    fragment_serverTimestamp,
    fragment_fragmentLengthInMilliseconds,

    -- ** FragmentSelector
    fragmentSelector_fragmentSelectorType,
    fragmentSelector_timestampRange,

    -- ** HLSFragmentSelector
    hLSFragmentSelector_fragmentSelectorType,
    hLSFragmentSelector_timestampRange,

    -- ** HLSTimestampRange
    hLSTimestampRange_endTimestamp,
    hLSTimestampRange_startTimestamp,

    -- ** TimestampRange
    timestampRange_startTimestamp,
    timestampRange_endTimestamp,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.GetClip
import Network.AWS.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
import Network.AWS.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
import Network.AWS.KinesisVideoArchivedMedia.GetMediaForFragmentList
import Network.AWS.KinesisVideoArchivedMedia.ListFragments
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.Fragment
import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelector
import Network.AWS.KinesisVideoArchivedMedia.Types.HLSTimestampRange
import Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
