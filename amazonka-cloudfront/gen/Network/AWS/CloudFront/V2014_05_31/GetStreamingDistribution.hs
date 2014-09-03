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
    , getStreamingDistribution
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

-- | Minimum specification for a 'GetStreamingDistribution' request.
getStreamingDistribution :: Text -- ^ 'gsdrId'
                         -> GetStreamingDistribution
getStreamingDistribution p1 = GetStreamingDistribution
    { _gsdrId = p1
    }

data GetStreamingDistribution = GetStreamingDistribution
    { _gsdrId :: Text
      -- ^ The streaming distribution's id.
    } deriving (Show, Generic)

-- | The streaming distribution's id.
gsdrId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetStreamingDistribution
    -> f GetStreamingDistribution
gsdrId f x =
    (\y -> x { _gsdrId = y })
       <$> f (_gsdrId x)
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
gsdsStreamingDistribution
    :: Functor f
    => (Maybe StreamingDistribution
    -> f (Maybe StreamingDistribution))
    -> GetStreamingDistributionResponse
    -> f GetStreamingDistributionResponse
gsdsStreamingDistribution f x =
    (\y -> x { _gsdsStreamingDistribution = y })
       <$> f (_gsdsStreamingDistribution x)
{-# INLINE gsdsStreamingDistribution #-}

-- | The current version of the streaming distribution's information. For
-- example: E2QWRUHAPOMQZL.
gsdsETag
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStreamingDistributionResponse
    -> f GetStreamingDistributionResponse
gsdsETag f x =
    (\y -> x { _gsdsETag = y })
       <$> f (_gsdsETag x)
{-# INLINE gsdsETag #-}

instance AWSRequest GetStreamingDistribution where
    type Sv GetStreamingDistribution = CloudFront
    type Rs GetStreamingDistribution = GetStreamingDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
