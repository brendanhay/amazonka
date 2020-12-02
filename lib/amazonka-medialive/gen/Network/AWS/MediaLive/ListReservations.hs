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
-- Module      : Network.AWS.MediaLive.ListReservations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List purchased reservations.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListReservations
  ( -- * Creating a Request
    listReservations,
    ListReservations,

    -- * Request Lenses
    lrVideoQuality,
    lrMaximumFramerate,
    lrResourceType,
    lrResolution,
    lrCodec,
    lrNextToken,
    lrSpecialFeature,
    lrChannelClass,
    lrMaximumBitrate,
    lrMaxResults,

    -- * Destructuring the Response
    listReservationsResponse,
    ListReservationsResponse,

    -- * Response Lenses
    lrrsNextToken,
    lrrsReservations,
    lrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for ListReservationsRequest
--
-- /See:/ 'listReservations' smart constructor.
data ListReservations = ListReservations'
  { _lrVideoQuality ::
      !(Maybe Text),
    _lrMaximumFramerate :: !(Maybe Text),
    _lrResourceType :: !(Maybe Text),
    _lrResolution :: !(Maybe Text),
    _lrCodec :: !(Maybe Text),
    _lrNextToken :: !(Maybe Text),
    _lrSpecialFeature :: !(Maybe Text),
    _lrChannelClass :: !(Maybe Text),
    _lrMaximumBitrate :: !(Maybe Text),
    _lrMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReservations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrVideoQuality' - Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
--
-- * 'lrMaximumFramerate' - Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
--
-- * 'lrResourceType' - Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- * 'lrResolution' - Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
--
-- * 'lrCodec' - Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
--
-- * 'lrNextToken' - Undocumented member.
--
-- * 'lrSpecialFeature' - Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
--
-- * 'lrChannelClass' - Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
--
-- * 'lrMaximumBitrate' - Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
--
-- * 'lrMaxResults' - Undocumented member.
listReservations ::
  ListReservations
listReservations =
  ListReservations'
    { _lrVideoQuality = Nothing,
      _lrMaximumFramerate = Nothing,
      _lrResourceType = Nothing,
      _lrResolution = Nothing,
      _lrCodec = Nothing,
      _lrNextToken = Nothing,
      _lrSpecialFeature = Nothing,
      _lrChannelClass = Nothing,
      _lrMaximumBitrate = Nothing,
      _lrMaxResults = Nothing
    }

-- | Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
lrVideoQuality :: Lens' ListReservations (Maybe Text)
lrVideoQuality = lens _lrVideoQuality (\s a -> s {_lrVideoQuality = a})

-- | Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
lrMaximumFramerate :: Lens' ListReservations (Maybe Text)
lrMaximumFramerate = lens _lrMaximumFramerate (\s a -> s {_lrMaximumFramerate = a})

-- | Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
lrResourceType :: Lens' ListReservations (Maybe Text)
lrResourceType = lens _lrResourceType (\s a -> s {_lrResourceType = a})

-- | Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
lrResolution :: Lens' ListReservations (Maybe Text)
lrResolution = lens _lrResolution (\s a -> s {_lrResolution = a})

-- | Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
lrCodec :: Lens' ListReservations (Maybe Text)
lrCodec = lens _lrCodec (\s a -> s {_lrCodec = a})

-- | Undocumented member.
lrNextToken :: Lens' ListReservations (Maybe Text)
lrNextToken = lens _lrNextToken (\s a -> s {_lrNextToken = a})

-- | Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
lrSpecialFeature :: Lens' ListReservations (Maybe Text)
lrSpecialFeature = lens _lrSpecialFeature (\s a -> s {_lrSpecialFeature = a})

-- | Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
lrChannelClass :: Lens' ListReservations (Maybe Text)
lrChannelClass = lens _lrChannelClass (\s a -> s {_lrChannelClass = a})

-- | Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
lrMaximumBitrate :: Lens' ListReservations (Maybe Text)
lrMaximumBitrate = lens _lrMaximumBitrate (\s a -> s {_lrMaximumBitrate = a})

-- | Undocumented member.
lrMaxResults :: Lens' ListReservations (Maybe Natural)
lrMaxResults = lens _lrMaxResults (\s a -> s {_lrMaxResults = a}) . mapping _Nat

instance AWSPager ListReservations where
  page rq rs
    | stop (rs ^. lrrsNextToken) = Nothing
    | stop (rs ^. lrrsReservations) = Nothing
    | otherwise = Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListReservations where
  type Rs ListReservations = ListReservationsResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          ListReservationsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "reservations" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListReservations

instance NFData ListReservations

instance ToHeaders ListReservations where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListReservations where
  toPath = const "/prod/reservations"

instance ToQuery ListReservations where
  toQuery ListReservations' {..} =
    mconcat
      [ "videoQuality" =: _lrVideoQuality,
        "maximumFramerate" =: _lrMaximumFramerate,
        "resourceType" =: _lrResourceType,
        "resolution" =: _lrResolution,
        "codec" =: _lrCodec,
        "nextToken" =: _lrNextToken,
        "specialFeature" =: _lrSpecialFeature,
        "channelClass" =: _lrChannelClass,
        "maximumBitrate" =: _lrMaximumBitrate,
        "maxResults" =: _lrMaxResults
      ]

-- | Placeholder documentation for ListReservationsResponse
--
-- /See:/ 'listReservationsResponse' smart constructor.
data ListReservationsResponse = ListReservationsResponse'
  { _lrrsNextToken ::
      !(Maybe Text),
    _lrrsReservations ::
      !(Maybe [Reservation]),
    _lrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReservationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsNextToken' - Token to retrieve the next page of results
--
-- * 'lrrsReservations' - List of reservations
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listReservationsResponse ::
  -- | 'lrrsResponseStatus'
  Int ->
  ListReservationsResponse
listReservationsResponse pResponseStatus_ =
  ListReservationsResponse'
    { _lrrsNextToken = Nothing,
      _lrrsReservations = Nothing,
      _lrrsResponseStatus = pResponseStatus_
    }

-- | Token to retrieve the next page of results
lrrsNextToken :: Lens' ListReservationsResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\s a -> s {_lrrsNextToken = a})

-- | List of reservations
lrrsReservations :: Lens' ListReservationsResponse [Reservation]
lrrsReservations = lens _lrrsReservations (\s a -> s {_lrrsReservations = a}) . _Default . _Coerce

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListReservationsResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\s a -> s {_lrrsResponseStatus = a})

instance NFData ListReservationsResponse
