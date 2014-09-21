{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.ReadPreset
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
module Network.AWS.ElasticTranscoder.ReadPreset
    (
    -- * Request
      ReadPreset
    -- ** Request constructor
    , readPreset
    -- ** Request lenses
    , rp1Id

    -- * Response
    , ReadPresetResponse
    -- ** Response constructor
    , readPresetResponse
    -- ** Response lenses
    , rprrPreset
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The ReadPresetRequest structure.
newtype ReadPreset = ReadPreset
    { _rp1Id :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReadPreset' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
readPreset :: Text -- ^ 'rp1Id'
           -> ReadPreset
readPreset p1 = ReadPreset
    { _rp1Id = p1
    }

-- | The identifier of the preset for which you want to get detailed
-- information.
rp1Id :: Lens' ReadPreset Text
rp1Id = lens _rp1Id (\s a -> s { _rp1Id = a })

instance ToPath ReadPreset

instance ToQuery ReadPreset

instance ToHeaders ReadPreset

instance ToJSON ReadPreset

-- | The ReadPresetResponse structure.
newtype ReadPresetResponse = ReadPresetResponse
    { _rprrPreset :: Maybe Preset
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReadPresetResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Preset ::@ @Maybe Preset@
--
readPresetResponse :: ReadPresetResponse
readPresetResponse = ReadPresetResponse
    { _rprrPreset = Nothing
    }

-- | A section of the response body that provides information about the preset.
rprrPreset :: Lens' ReadPresetResponse (Maybe Preset)
rprrPreset = lens _rprrPreset (\s a -> s { _rprrPreset = a })

instance FromJSON ReadPresetResponse

instance AWSRequest ReadPreset where
    type Sv ReadPreset = ElasticTranscoder
    type Rs ReadPreset = ReadPresetResponse

    request = get
    response _ = jsonResponse
