{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.ElasticTranscoder.V2012_09_25.ListPresets where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.ElasticTranscoder.V2012_09_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'ListPresets' request.
listPresets :: ListPresets
listPresets = ListPresets
    { _lptAscending = Nothing
    , _lptPageToken = Nothing
    }

data ListPresets = ListPresets
    { _lptAscending :: Maybe Text
      -- ^ To list presets in chronological order by the date and time that
      -- they were created, enter true. To list presets in reverse
      -- chronological order, enter false.
    , _lptPageToken :: Maybe Text
      -- ^ When Elastic Transcoder returns more than one page of results,
      -- use pageToken in subsequent GET requests to get each successive
      -- page of results.
    } deriving (Show, Generic)

makeLenses ''ListPresets

instance ToPath ListPresets where
    toPath = const "/2012-09-25/presets"

instance ToQuery ListPresets where
    toQuery ListPresets{..} = mconcat
        [ "Ascending" =? _lptAscending
        , "PageToken" =? _lptPageToken
        ]

instance ToHeaders ListPresets

instance ToJSON ListPresets

data ListPresetsResponse = ListPresetsResponse
    { _lpuNextPageToken :: Maybe Text
      -- ^ A value that you use to access the second and subsequent pages of
      -- results, if any. When the presets fit on one page or when you've
      -- reached the last page of results, the value of NextPageToken is
      -- null.
    , _lpuPresets :: [Preset]
      -- ^ An array of Preset objects.
    } deriving (Show, Generic)

makeLenses ''ListPresetsResponse

instance FromJSON ListPresetsResponse

instance AWSRequest ListPresets where
    type Sv ListPresets = ElasticTranscoder
    type Rs ListPresets = ListPresetsResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListPresets where
    next rq rs = (\x -> rq { _lptPageToken = Just x })
        <$> (_lpuNextPageToken rs)
