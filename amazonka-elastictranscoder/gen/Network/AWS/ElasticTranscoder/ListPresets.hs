{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListPresets operation gets a list of the default presets included with
-- Elastic Transcoder and the presets that you've added in an AWS region. GET
-- /2012-09-25/presets HTTP/1.1 Content-Type: charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Presets":[ { "Audio":{ "BitRate":"96", "Channels":"2",
-- "Codec":"AAC", "SampleRate":"44100" }, "Container":"mp4",
-- "Description":"Use for published videos", "Id":"5555555555555-abcde5",
-- "Name":"DefaultPreset", "Thumbnails":{ "Format":"png", "Interval":"120",
-- "MaxHeight":"auto", "MaxWidth":"auto", "PaddingPolicy":"Pad",
-- "SizingPolicy":"Fit" }, "Type":"Custom", "Video":{ "BitRate":"1600",
-- "Codec":"H.264", "CodecOptions":{ "Level":"2.2", "MaxReferenceFrames":"3",
-- "Profile":"main", "MaxBitRate":"", "BufferSize":"" },
-- "DisplayAspectRatio":"auto", "FixedGOP":"false", "FrameRate":"30",
-- "KeyframesMaxDist":"240", "MaxHeight":"auto", "MaxWidth":"auto",
-- "PaddingPolicy":"Pad", "SizingPolicy":"Fit" } }, {...} ] }.
module Network.AWS.ElasticTranscoder
    (
    -- * Request
      ListPresets
    -- ** Request constructor
    , mkListPresets
    -- ** Request lenses
    , lp1Ascending
    , lp1PageToken

    -- * Response
    , ListPresetsResponse
    -- ** Response constructor
    , mkListPresetsResponse
    -- ** Response lenses
    , lprrPresets
    , lprrNextPageToken
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The ListPresetsRequest structure.
data ListPresets = ListPresets
    { _lp1Ascending :: Maybe Text
    , _lp1PageToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPresets' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Ascending ::@ @Maybe Text@
--
-- * @PageToken ::@ @Maybe Text@
--
mkListPresets :: ListPresets
mkListPresets = ListPresets
    { _lp1Ascending = Nothing
    , _lp1PageToken = Nothing
    }

-- | To list presets in chronological order by the date and time that they were
-- created, enter true. To list presets in reverse chronological order, enter
-- false.
lp1Ascending :: Lens' ListPresets (Maybe Text)
lp1Ascending = lens _lp1Ascending (\s a -> s { _lp1Ascending = a })

-- | When Elastic Transcoder returns more than one page of results, use
-- pageToken in subsequent GET requests to get each successive page of
-- results.
lp1PageToken :: Lens' ListPresets (Maybe Text)
lp1PageToken = lens _lp1PageToken (\s a -> s { _lp1PageToken = a })

instance ToPath ListPresets

instance ToQuery ListPresets

instance ToHeaders ListPresets

instance ToJSON ListPresets

-- | The ListPresetsResponse structure.
data ListPresetsResponse = ListPresetsResponse
    { _lprrPresets :: [Preset]
    , _lprrNextPageToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPresetsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Presets ::@ @[Preset]@
--
-- * @NextPageToken ::@ @Maybe Text@
--
mkListPresetsResponse :: ListPresetsResponse
mkListPresetsResponse = ListPresetsResponse
    { _lprrPresets = mempty
    , _lprrNextPageToken = Nothing
    }

-- | An array of Preset objects.
lprrPresets :: Lens' ListPresetsResponse [Preset]
lprrPresets = lens _lprrPresets (\s a -> s { _lprrPresets = a })

-- | A value that you use to access the second and subsequent pages of results,
-- if any. When the presets fit on one page or when you've reached the last
-- page of results, the value of NextPageToken is null.
lprrNextPageToken :: Lens' ListPresetsResponse (Maybe Text)
lprrNextPageToken =
    lens _lprrNextPageToken (\s a -> s { _lprrNextPageToken = a })

instance FromJSON ListPresetsResponse

instance AWSRequest ListPresets where
    type Sv ListPresets = ElasticTranscoder
    type Rs ListPresets = ListPresetsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListPresets where
    next rq rs = (\x -> rq & lp1PageToken ?~ x)
        <$> (rs ^. lprrNextPageToken)
