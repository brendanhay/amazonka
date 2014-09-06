{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a streaming distribution.
module Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution
    (
    -- * Request
      GetStreamingDistribution
    -- ** Request constructor
    , mkGetStreamingDistribution
    -- ** Request lenses
    , gsdId

    -- * Response
    , GetStreamingDistributionResponse
    -- ** Response lenses
    , gsdrsStreamingDistribution
    , gsdrsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get a streaming distribution's information.
newtype GetStreamingDistribution = GetStreamingDistribution
    { _gsdId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStreamingDistribution' request.
mkGetStreamingDistribution :: Text -- ^ 'gsdId'
                           -> GetStreamingDistribution
mkGetStreamingDistribution p1 = GetStreamingDistribution
    { _gsdId = p1
    }
{-# INLINE mkGetStreamingDistribution #-}

-- | The streaming distribution's id.
gsdId :: Lens' GetStreamingDistribution Text
gsdId = lens _gsdId (\s a -> s { _gsdId = a })
{-# INLINE gsdId #-}

instance ToPath GetStreamingDistribution where
    toPath GetStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _gsdId
        ]

instance ToQuery GetStreamingDistribution

instance ToHeaders GetStreamingDistribution

instance ToXML GetStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetStreamingDistributionRequest"

-- | The returned result of the corresponding request.
data GetStreamingDistributionResponse = GetStreamingDistributionResponse
    { _gsdrsStreamingDistribution :: Maybe StreamingDistribution
    , _gsdrsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The streaming distribution's information.
gsdrsStreamingDistribution :: Lens' GetStreamingDistributionResponse (Maybe StreamingDistribution)
gsdrsStreamingDistribution =
    lens _gsdrsStreamingDistribution
         (\s a -> s { _gsdrsStreamingDistribution = a })
{-# INLINE gsdrsStreamingDistribution #-}

-- | The current version of the streaming distribution's information. For
-- example: E2QWRUHAPOMQZL.
gsdrsETag :: Lens' GetStreamingDistributionResponse (Maybe Text)
gsdrsETag = lens _gsdrsETag (\s a -> s { _gsdrsETag = a })
{-# INLINE gsdrsETag #-}

instance AWSRequest GetStreamingDistribution where
    type Sv GetStreamingDistribution = CloudFront
    type Rs GetStreamingDistribution = GetStreamingDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
