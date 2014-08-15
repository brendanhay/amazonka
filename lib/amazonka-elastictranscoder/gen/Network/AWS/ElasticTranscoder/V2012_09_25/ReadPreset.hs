{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.ReadPreset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ReadPreset operation gets detailed information about a preset. GET
-- /2012-09-25/presets/5555555555555-abcde5 HTTP/1.1 Content-Type:
-- application/json; charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] Status: 200 OK Content-Type:
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] { "Preset":{ "Audio":{ "Codec":"AAC",
-- "SampleRate":"44100", "BitRate":96, "Channels":2 }, "Container":"mp4",
-- "Description":"Use for published videos", "Id":"5555555555555-abcde5",
-- "Name":"DefaultPreset", "Thumbnails":{ "Format":"png", "Interval":"120",
-- "MaxWidth": "auto", "MaxHeight": "auto", "SizingPolicy": "Fill",
-- "PaddingPolicy": "Pad" }, "Type":"Custom", "Video":{ "MaxWidth": "auto",
-- "MaxHeight": "auto", "SizingPolicy": "Fill", "PaddingPolicy": "Pad",
-- "DisplayAspectRatio": "auto", "BitRate":"1600", "Codec":"H.264",
-- "CodecOptions":{ "Level":"2.2", "MaxReferenceFrames":"3", "Profile":"main",
-- "MaxBitRate":"", "BufferSize":"" }, "FixedGOP":"false", "FrameRate":"30",
-- "KeyframesMaxDist":"240" } } }.
module Network.AWS.ElasticTranscoder.V2012_09_25.ReadPreset where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.ElasticTranscoder.V2012_09_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data ReadPreset = ReadPreset
    { _rprId :: Text
      -- ^ The identifier of the preset for which you want to get detailed
      -- information.
    } deriving (Show, Generic)

makeLenses ''ReadPreset

instance ToPath ReadPreset where
    toPath ReadPreset{..} = mconcat
        [ "/2012-09-25/presets/"
        , toBS _rprId
        ]

instance ToQuery ReadPreset

instance ToHeaders ReadPreset

instance ToJSON ReadPreset

data ReadPresetResponse = ReadPresetResponse
    { _rpsPreset :: Maybe Preset
      -- ^ A section of the response body that provides information about
      -- the preset.
    } deriving (Show, Generic)

makeLenses ''ReadPresetResponse

instance FromJSON ReadPresetResponse

instance AWSRequest ReadPreset where
    type Sv ReadPreset = ElasticTranscoder
    type Rs ReadPreset = ReadPresetResponse

    request = get
    response _ = jsonResponse
