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
    , mkGetStreamingDistributionRequest
    -- ** Request lenses
    , gsdrId

    -- * Response
    , GetStreamingDistributionResponse
    -- ** Response lenses
    , gsdsStreamingDistribution
    , gsdsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStreamingDistribution' request.
mkGetStreamingDistributionRequest :: Text -- ^ 'gsdrId'
                                  -> GetStreamingDistribution
mkGetStreamingDistributionRequest p1 = GetStreamingDistribution
    { _gsdrId = p1
    }
{-# INLINE mkGetStreamingDistributionRequest #-}

newtype GetStreamingDistribution = GetStreamingDistribution
    { _gsdrId :: Text
      -- ^ The streaming distribution's id.
    } deriving (Show, Generic)

-- | The streaming distribution's id.
gsdrId :: Lens' GetStreamingDistribution (Text)
gsdrId = lens _gsdrId (\s a -> s { _gsdrId = a })
{-# INLINE gsdrId #-}

instance ToPath GetStreamingDistribution where
    toPath GetStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _gsdrId
        ]

instance ToQuery GetStreamingDistribution

instance ToHeaders GetStreamingDistribution

instance ToXML GetStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetStreamingDistributionRequest"

data GetStreamingDistributionResponse = GetStreamingDistributionResponse
    { _gsdsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    , _gsdsETag :: Maybe Text
      -- ^ The current version of the streaming distribution's information.
      -- For example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The streaming distribution's information.
gsdsStreamingDistribution :: Lens' GetStreamingDistributionResponse (Maybe StreamingDistribution)
gsdsStreamingDistribution = lens _gsdsStreamingDistribution (\s a -> s { _gsdsStreamingDistribution = a })
{-# INLINE gsdsStreamingDistribution #-}

-- | The current version of the streaming distribution's information. For
-- example: E2QWRUHAPOMQZL.
gsdsETag :: Lens' GetStreamingDistributionResponse (Maybe Text)
gsdsETag = lens _gsdsETag (\s a -> s { _gsdsETag = a })
{-# INLINE gsdsETag #-}

instance AWSRequest GetStreamingDistribution where
    type Sv GetStreamingDistribution = CloudFront
    type Rs GetStreamingDistribution = GetStreamingDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
