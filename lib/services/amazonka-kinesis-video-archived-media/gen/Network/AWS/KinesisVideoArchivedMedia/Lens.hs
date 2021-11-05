{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoArchivedMedia.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Lens
  ( -- * Operations

    -- ** GetHLSStreamingSessionURL
    getHLSStreamingSessionURL_displayFragmentTimestamp,
    getHLSStreamingSessionURL_hLSFragmentSelector,
    getHLSStreamingSessionURL_expires,
    getHLSStreamingSessionURL_streamARN,
    getHLSStreamingSessionURL_playbackMode,
    getHLSStreamingSessionURL_containerFormat,
    getHLSStreamingSessionURL_maxMediaPlaylistFragmentResults,
    getHLSStreamingSessionURL_discontinuityMode,
    getHLSStreamingSessionURL_streamName,
    getHLSStreamingSessionURLResponse_hLSStreamingSessionURL,
    getHLSStreamingSessionURLResponse_httpStatus,

    -- ** GetClip
    getClip_streamARN,
    getClip_streamName,
    getClip_clipFragmentSelector,
    getClipResponse_contentType,
    getClipResponse_httpStatus,
    getClipResponse_payload,

    -- ** GetMediaForFragmentList
    getMediaForFragmentList_streamARN,
    getMediaForFragmentList_streamName,
    getMediaForFragmentList_fragments,
    getMediaForFragmentListResponse_contentType,
    getMediaForFragmentListResponse_httpStatus,
    getMediaForFragmentListResponse_payload,

    -- ** ListFragments
    listFragments_fragmentSelector,
    listFragments_streamARN,
    listFragments_nextToken,
    listFragments_streamName,
    listFragments_maxResults,
    listFragmentsResponse_nextToken,
    listFragmentsResponse_fragments,
    listFragmentsResponse_httpStatus,

    -- ** GetDASHStreamingSessionURL
    getDASHStreamingSessionURL_displayFragmentTimestamp,
    getDASHStreamingSessionURL_expires,
    getDASHStreamingSessionURL_dASHFragmentSelector,
    getDASHStreamingSessionURL_maxManifestFragmentResults,
    getDASHStreamingSessionURL_streamARN,
    getDASHStreamingSessionURL_playbackMode,
    getDASHStreamingSessionURL_streamName,
    getDASHStreamingSessionURL_displayFragmentNumber,
    getDASHStreamingSessionURLResponse_dASHStreamingSessionURL,
    getDASHStreamingSessionURLResponse_httpStatus,

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
    fragment_serverTimestamp,
    fragment_fragmentSizeInBytes,
    fragment_fragmentNumber,
    fragment_producerTimestamp,

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

import Amazonka.KinesisVideoArchivedMedia.GetClip
import Amazonka.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
import Amazonka.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
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
import Amazonka.KinesisVideoArchivedMedia.Types.TimestampRange
