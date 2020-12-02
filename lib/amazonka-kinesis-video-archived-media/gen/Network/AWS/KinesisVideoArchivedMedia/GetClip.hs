{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetClip
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads an MP4 file (clip) containing the archived, on-demand media from the specified video stream over the specified time range.
--
--
-- Both the StreamName and the StreamARN parameters are optional, but you must specify either the StreamName or the StreamARN when invoking this API operation.
--
-- As a prerequsite to using GetCLip API, you must obtain an endpoint using @GetDataEndpoint@ , specifying GET_CLIP forthe @APIName@ parameter.
--
-- An Amazon Kinesis video stream has the following requirements for providing data through MP4:
--
--     * The media must contain h.264 or h.265 encoded video and, optionally, AAC or G.711 encoded audio. Specifically, the codec ID of track 1 should be @V_MPEG/ISO/AVC@ (for h.264) or V_MPEGH/ISO/HEVC (for H.265). Optionally, the codec ID of track 2 should be @A_AAC@ (for AAC) or A_MS/ACM (for G.711).
--
--     * Data retention must be greater than 0.
--
--     * The video track of each fragment must contain codec private data in the Advanced Video Coding (AVC) for H.264 format and HEVC for H.265 format. For more information, see <https://www.iso.org/standard/55980.html MPEG-4 specification ISO/IEC 14496-15> . For information about adapting stream data to a given format, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags> .
--
--     * The audio track (if present) of each fragment must contain codec private data in the AAC format (<https://www.iso.org/standard/43345.html AAC specification ISO/IEC 13818-7> ) or the <http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html MS Wave format> .
--
--
--
-- You can monitor the amount of outgoing data by monitoring the @GetClip.OutgoingBytes@ Amazon CloudWatch metric. For information about using CloudWatch to monitor Kinesis Video Streams, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/monitoring.html Monitoring Kinesis Video Streams> . For pricing information, see <https://aws.amazon.com/kinesis/video-streams/pricing/ Amazon Kinesis Video Streams Pricing> and <https://aws.amazon.com/pricing/ AWS Pricing> . Charges for outgoing AWS data apply.
module Network.AWS.KinesisVideoArchivedMedia.GetClip
  ( -- * Creating a Request
    getClip,
    GetClip,

    -- * Request Lenses
    gcStreamARN,
    gcStreamName,
    gcClipFragmentSelector,

    -- * Destructuring the Response
    getClipResponse,
    GetClipResponse,

    -- * Response Lenses
    gcrsContentType,
    gcrsResponseStatus,
    gcrsPayload,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getClip' smart constructor.
data GetClip = GetClip'
  { _gcStreamARN :: !(Maybe Text),
    _gcStreamName :: !(Maybe Text),
    _gcClipFragmentSelector :: !ClipFragmentSelector
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetClip' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcStreamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the media clip.  You must specify either the StreamName or the StreamARN.
--
-- * 'gcStreamName' - The name of the stream for which to retrieve the media clip.  You must specify either the StreamName or the StreamARN.
--
-- * 'gcClipFragmentSelector' - The time range of the requested clip and the source of the timestamps.
getClip ::
  -- | 'gcClipFragmentSelector'
  ClipFragmentSelector ->
  GetClip
getClip pClipFragmentSelector_ =
  GetClip'
    { _gcStreamARN = Nothing,
      _gcStreamName = Nothing,
      _gcClipFragmentSelector = pClipFragmentSelector_
    }

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the media clip.  You must specify either the StreamName or the StreamARN.
gcStreamARN :: Lens' GetClip (Maybe Text)
gcStreamARN = lens _gcStreamARN (\s a -> s {_gcStreamARN = a})

-- | The name of the stream for which to retrieve the media clip.  You must specify either the StreamName or the StreamARN.
gcStreamName :: Lens' GetClip (Maybe Text)
gcStreamName = lens _gcStreamName (\s a -> s {_gcStreamName = a})

-- | The time range of the requested clip and the source of the timestamps.
gcClipFragmentSelector :: Lens' GetClip ClipFragmentSelector
gcClipFragmentSelector = lens _gcClipFragmentSelector (\s a -> s {_gcClipFragmentSelector = a})

instance AWSRequest GetClip where
  type Rs GetClip = GetClipResponse
  request = postJSON kinesisVideoArchivedMedia
  response =
    receiveBody
      ( \s h x ->
          GetClipResponse'
            <$> (h .#? "Content-Type") <*> (pure (fromEnum s)) <*> (pure x)
      )

instance Hashable GetClip

instance NFData GetClip

instance ToHeaders GetClip where
  toHeaders = const mempty

instance ToJSON GetClip where
  toJSON GetClip' {..} =
    object
      ( catMaybes
          [ ("StreamARN" .=) <$> _gcStreamARN,
            ("StreamName" .=) <$> _gcStreamName,
            Just ("ClipFragmentSelector" .= _gcClipFragmentSelector)
          ]
      )

instance ToPath GetClip where
  toPath = const "/getClip"

instance ToQuery GetClip where
  toQuery = const mempty

-- | /See:/ 'getClipResponse' smart constructor.
data GetClipResponse = GetClipResponse'
  { _gcrsContentType ::
      !(Maybe Text),
    _gcrsResponseStatus :: !Int,
    _gcrsPayload :: !RsBody
  }
  deriving (Show, Generic)

-- | Creates a value of 'GetClipResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsContentType' - The content type of the media in the requested clip.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
--
-- * 'gcrsPayload' - Traditional MP4 file that contains the media clip from the specified video stream. The output will contain the first 100 MB or the first 200 fragments from the specified start timestamp. For more information, see <Kinesis Video Streams Limits Kinesis Video Streams Limits> .
getClipResponse ::
  -- | 'gcrsResponseStatus'
  Int ->
  -- | 'gcrsPayload'
  RsBody ->
  GetClipResponse
getClipResponse pResponseStatus_ pPayload_ =
  GetClipResponse'
    { _gcrsContentType = Nothing,
      _gcrsResponseStatus = pResponseStatus_,
      _gcrsPayload = pPayload_
    }

-- | The content type of the media in the requested clip.
gcrsContentType :: Lens' GetClipResponse (Maybe Text)
gcrsContentType = lens _gcrsContentType (\s a -> s {_gcrsContentType = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetClipResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\s a -> s {_gcrsResponseStatus = a})

-- | Traditional MP4 file that contains the media clip from the specified video stream. The output will contain the first 100 MB or the first 200 fragments from the specified start timestamp. For more information, see <Kinesis Video Streams Limits Kinesis Video Streams Limits> .
gcrsPayload :: Lens' GetClipResponse RsBody
gcrsPayload = lens _gcrsPayload (\s a -> s {_gcrsPayload = a})
