{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.DeletePreset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeletePreset operation removes a preset that you've added in an AWS
-- region. You can't delete the default presets that are included with Elastic
-- Transcoder. DELETE /2012-09-25/pipelines/5555555555555-abcde5 HTTP/1.1
-- Content-Type: charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 202 Accepted x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Success":"true" }.
module Network.AWS.ElasticTranscoder.V2012_09_25.DeletePreset
    (
    -- * Request
      DeletePreset
    -- ** Request constructor
    , mkDeletePreset
    -- ** Request lenses
    , dp1Id

    -- * Response
    , DeletePresetResponse
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The DeletePresetRequest structure.
newtype DeletePreset = DeletePreset
    { _dp1Id :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePreset' request.
mkDeletePreset :: Text -- ^ 'dp1Id'
               -> DeletePreset
mkDeletePreset p1 = DeletePreset
    { _dp1Id = p1
    }
{-# INLINE mkDeletePreset #-}

-- | The identifier of the preset for which you want to get detailed
-- information.
dp1Id :: Lens' DeletePreset Text
dp1Id = lens _dp1Id (\s a -> s { _dp1Id = a })
{-# INLINE dp1Id #-}

instance ToPath DeletePreset where
    toPath DeletePreset{..} = mconcat
        [ "/2012-09-25/presets/"
        , toBS _dp1Id
        ]

instance ToQuery DeletePreset

instance ToHeaders DeletePreset

instance ToJSON DeletePreset

-- | The DeletePresetResponse structure.
data DeletePresetResponse = DeletePresetResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeletePreset where
    type Sv DeletePreset = ElasticTranscoder
    type Rs DeletePreset = DeletePresetResponse

    request = delete
    response _ = nullaryResponse DeletePresetResponse
