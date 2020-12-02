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
-- Module      : Network.AWS.MediaLive.ListOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List offerings available for purchase.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListOfferings
  ( -- * Creating a Request
    listOfferings,
    ListOfferings,

    -- * Request Lenses
    loVideoQuality,
    loMaximumFramerate,
    loResourceType,
    loChannelConfiguration,
    loResolution,
    loCodec,
    loNextToken,
    loSpecialFeature,
    loChannelClass,
    loMaximumBitrate,
    loDuration,
    loMaxResults,

    -- * Destructuring the Response
    listOfferingsResponse,
    ListOfferingsResponse,

    -- * Response Lenses
    lorsNextToken,
    lorsOfferings,
    lorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for ListOfferingsRequest
--
-- /See:/ 'listOfferings' smart constructor.
data ListOfferings = ListOfferings'
  { _loVideoQuality ::
      !(Maybe Text),
    _loMaximumFramerate :: !(Maybe Text),
    _loResourceType :: !(Maybe Text),
    _loChannelConfiguration :: !(Maybe Text),
    _loResolution :: !(Maybe Text),
    _loCodec :: !(Maybe Text),
    _loNextToken :: !(Maybe Text),
    _loSpecialFeature :: !(Maybe Text),
    _loChannelClass :: !(Maybe Text),
    _loMaximumBitrate :: !(Maybe Text),
    _loDuration :: !(Maybe Text),
    _loMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loVideoQuality' - Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
--
-- * 'loMaximumFramerate' - Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
--
-- * 'loResourceType' - Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- * 'loChannelConfiguration' - Filter to offerings that match the configuration of an existing channel, e.g. '2345678' (a channel ID)
--
-- * 'loResolution' - Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
--
-- * 'loCodec' - Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
--
-- * 'loNextToken' - Undocumented member.
--
-- * 'loSpecialFeature' - Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
--
-- * 'loChannelClass' - Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
--
-- * 'loMaximumBitrate' - Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
--
-- * 'loDuration' - Filter by offering duration, e.g. '12'
--
-- * 'loMaxResults' - Undocumented member.
listOfferings ::
  ListOfferings
listOfferings =
  ListOfferings'
    { _loVideoQuality = Nothing,
      _loMaximumFramerate = Nothing,
      _loResourceType = Nothing,
      _loChannelConfiguration = Nothing,
      _loResolution = Nothing,
      _loCodec = Nothing,
      _loNextToken = Nothing,
      _loSpecialFeature = Nothing,
      _loChannelClass = Nothing,
      _loMaximumBitrate = Nothing,
      _loDuration = Nothing,
      _loMaxResults = Nothing
    }

-- | Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
loVideoQuality :: Lens' ListOfferings (Maybe Text)
loVideoQuality = lens _loVideoQuality (\s a -> s {_loVideoQuality = a})

-- | Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
loMaximumFramerate :: Lens' ListOfferings (Maybe Text)
loMaximumFramerate = lens _loMaximumFramerate (\s a -> s {_loMaximumFramerate = a})

-- | Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
loResourceType :: Lens' ListOfferings (Maybe Text)
loResourceType = lens _loResourceType (\s a -> s {_loResourceType = a})

-- | Filter to offerings that match the configuration of an existing channel, e.g. '2345678' (a channel ID)
loChannelConfiguration :: Lens' ListOfferings (Maybe Text)
loChannelConfiguration = lens _loChannelConfiguration (\s a -> s {_loChannelConfiguration = a})

-- | Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
loResolution :: Lens' ListOfferings (Maybe Text)
loResolution = lens _loResolution (\s a -> s {_loResolution = a})

-- | Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
loCodec :: Lens' ListOfferings (Maybe Text)
loCodec = lens _loCodec (\s a -> s {_loCodec = a})

-- | Undocumented member.
loNextToken :: Lens' ListOfferings (Maybe Text)
loNextToken = lens _loNextToken (\s a -> s {_loNextToken = a})

-- | Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
loSpecialFeature :: Lens' ListOfferings (Maybe Text)
loSpecialFeature = lens _loSpecialFeature (\s a -> s {_loSpecialFeature = a})

-- | Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
loChannelClass :: Lens' ListOfferings (Maybe Text)
loChannelClass = lens _loChannelClass (\s a -> s {_loChannelClass = a})

-- | Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
loMaximumBitrate :: Lens' ListOfferings (Maybe Text)
loMaximumBitrate = lens _loMaximumBitrate (\s a -> s {_loMaximumBitrate = a})

-- | Filter by offering duration, e.g. '12'
loDuration :: Lens' ListOfferings (Maybe Text)
loDuration = lens _loDuration (\s a -> s {_loDuration = a})

-- | Undocumented member.
loMaxResults :: Lens' ListOfferings (Maybe Natural)
loMaxResults = lens _loMaxResults (\s a -> s {_loMaxResults = a}) . mapping _Nat

instance AWSPager ListOfferings where
  page rq rs
    | stop (rs ^. lorsNextToken) = Nothing
    | stop (rs ^. lorsOfferings) = Nothing
    | otherwise = Just $ rq & loNextToken .~ rs ^. lorsNextToken

instance AWSRequest ListOfferings where
  type Rs ListOfferings = ListOfferingsResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          ListOfferingsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "offerings" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListOfferings

instance NFData ListOfferings

instance ToHeaders ListOfferings where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListOfferings where
  toPath = const "/prod/offerings"

instance ToQuery ListOfferings where
  toQuery ListOfferings' {..} =
    mconcat
      [ "videoQuality" =: _loVideoQuality,
        "maximumFramerate" =: _loMaximumFramerate,
        "resourceType" =: _loResourceType,
        "channelConfiguration" =: _loChannelConfiguration,
        "resolution" =: _loResolution,
        "codec" =: _loCodec,
        "nextToken" =: _loNextToken,
        "specialFeature" =: _loSpecialFeature,
        "channelClass" =: _loChannelClass,
        "maximumBitrate" =: _loMaximumBitrate,
        "duration" =: _loDuration,
        "maxResults" =: _loMaxResults
      ]

-- | Placeholder documentation for ListOfferingsResponse
--
-- /See:/ 'listOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { _lorsNextToken ::
      !(Maybe Text),
    _lorsOfferings :: !(Maybe [Offering]),
    _lorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorsNextToken' - Token to retrieve the next page of results
--
-- * 'lorsOfferings' - List of offerings
--
-- * 'lorsResponseStatus' - -- | The response status code.
listOfferingsResponse ::
  -- | 'lorsResponseStatus'
  Int ->
  ListOfferingsResponse
listOfferingsResponse pResponseStatus_ =
  ListOfferingsResponse'
    { _lorsNextToken = Nothing,
      _lorsOfferings = Nothing,
      _lorsResponseStatus = pResponseStatus_
    }

-- | Token to retrieve the next page of results
lorsNextToken :: Lens' ListOfferingsResponse (Maybe Text)
lorsNextToken = lens _lorsNextToken (\s a -> s {_lorsNextToken = a})

-- | List of offerings
lorsOfferings :: Lens' ListOfferingsResponse [Offering]
lorsOfferings = lens _lorsOfferings (\s a -> s {_lorsOfferings = a}) . _Default . _Coerce

-- | -- | The response status code.
lorsResponseStatus :: Lens' ListOfferingsResponse Int
lorsResponseStatus = lens _lorsResponseStatus (\s a -> s {_lorsResponseStatus = a})

instance NFData ListOfferingsResponse
