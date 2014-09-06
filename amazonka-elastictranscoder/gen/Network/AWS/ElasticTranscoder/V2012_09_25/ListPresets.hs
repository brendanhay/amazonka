{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.ListPresets
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
module Network.AWS.ElasticTranscoder.V2012_09_25.ListPresets
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
    -- ** Response lenses
    , lprsrsPresets
    , lprsrsNextPageToken
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The ListPresetsRequest structure.
data ListPresets = ListPresets
    { _lp1Ascending :: Maybe Text
    , _lp1PageToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPresets' request.
mkListPresets :: ListPresets
mkListPresets = ListPresets
    { _lp1Ascending = Nothing
    , _lp1PageToken = Nothing
    }
{-# INLINE mkListPresets #-}

-- | To list presets in chronological order by the date and time that they were
-- created, enter true. To list presets in reverse chronological order, enter
-- false.
lp1Ascending :: Lens' ListPresets (Maybe Text)
lp1Ascending = lens _lp1Ascending (\s a -> s { _lp1Ascending = a })
{-# INLINE lp1Ascending #-}

-- | When Elastic Transcoder returns more than one page of results, use
-- pageToken in subsequent GET requests to get each successive page of
-- results.
lp1PageToken :: Lens' ListPresets (Maybe Text)
lp1PageToken = lens _lp1PageToken (\s a -> s { _lp1PageToken = a })
{-# INLINE lp1PageToken #-}

instance ToPath ListPresets where
    toPath = const "/2012-09-25/presets"

instance ToQuery ListPresets where
    toQuery ListPresets{..} = mconcat
        [ "Ascending" =? _lp1Ascending
        , "PageToken" =? _lp1PageToken
        ]

instance ToHeaders ListPresets

instance ToJSON ListPresets

-- | The ListPresetsResponse structure.
data ListPresetsResponse = ListPresetsResponse
    { _lprsrsPresets :: [Preset]
    , _lprsrsNextPageToken :: Maybe Text
    } deriving (Show, Generic)

-- | An array of Preset objects.
lprsrsPresets :: Lens' ListPresetsResponse [Preset]
lprsrsPresets = lens _lprsrsPresets (\s a -> s { _lprsrsPresets = a })
{-# INLINE lprsrsPresets #-}

-- | A value that you use to access the second and subsequent pages of results,
-- if any. When the presets fit on one page or when you've reached the last
-- page of results, the value of NextPageToken is null.
lprsrsNextPageToken :: Lens' ListPresetsResponse (Maybe Text)
lprsrsNextPageToken =
    lens _lprsrsNextPageToken (\s a -> s { _lprsrsNextPageToken = a })
{-# INLINE lprsrsNextPageToken #-}

instance FromJSON ListPresetsResponse

instance AWSRequest ListPresets where
    type Sv ListPresets = ElasticTranscoder
    type Rs ListPresets = ListPresetsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListPresets where
    next rq rs = (\x -> rq { _lp1PageToken = Just x })
        <$> (_lprsrsNextPageToken rs)
