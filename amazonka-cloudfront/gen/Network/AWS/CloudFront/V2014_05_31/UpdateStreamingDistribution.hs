{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a streaming distribution.
module Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution
    (
    -- * Request
      UpdateStreamingDistribution
    -- ** Request constructor
    , updateStreamingDistribution
    -- ** Request lenses
    , usdrStreamingDistributionConfig
    , usdrId
    , usdrIfMatch

    -- * Response
    , UpdateStreamingDistributionResponse
    -- ** Response lenses
    , usdsStreamingDistribution
    , usdsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateStreamingDistribution' request.
updateStreamingDistribution :: StreamingDistributionConfig -- ^ 'usdrStreamingDistributionConfig'
                            -> Text -- ^ 'usdrId'
                            -> UpdateStreamingDistribution
updateStreamingDistribution p1 p2 = UpdateStreamingDistribution
    { _usdrStreamingDistributionConfig = p1
    , _usdrId = p2
    , _usdrIfMatch = Nothing
    }

data UpdateStreamingDistribution = UpdateStreamingDistribution
    { _usdrStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    , _usdrId :: Text
      -- ^ The streaming distribution's id.
    , _usdrIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when retrieving the
      -- streaming distribution's configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The streaming distribution's configuration information.
usdrStreamingDistributionConfig
    :: Functor f
    => (StreamingDistributionConfig
    -> f (StreamingDistributionConfig))
    -> UpdateStreamingDistribution
    -> f UpdateStreamingDistribution
usdrStreamingDistributionConfig f x =
    (\y -> x { _usdrStreamingDistributionConfig = y })
       <$> f (_usdrStreamingDistributionConfig x)
{-# INLINE usdrStreamingDistributionConfig #-}

-- | The streaming distribution's id.
usdrId
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateStreamingDistribution
    -> f UpdateStreamingDistribution
usdrId f x =
    (\y -> x { _usdrId = y })
       <$> f (_usdrId x)
{-# INLINE usdrId #-}

-- | The value of the ETag header you received when retrieving the streaming
-- distribution's configuration. For example: E2QWRUHAPOMQZL.
usdrIfMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStreamingDistribution
    -> f UpdateStreamingDistribution
usdrIfMatch f x =
    (\y -> x { _usdrIfMatch = y })
       <$> f (_usdrIfMatch x)
{-# INLINE usdrIfMatch #-}

instance ToPath UpdateStreamingDistribution where
    toPath UpdateStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _usdrId
        , "/config"
        ]

instance ToQuery UpdateStreamingDistribution

instance ToHeaders UpdateStreamingDistribution where
    toHeaders UpdateStreamingDistribution{..} = concat
        [ "If-Match" =: _usdrIfMatch
        ]

instance ToXML UpdateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateStreamingDistributionRequest"

data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse
    { _usdsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    , _usdsETag :: Maybe Text
      -- ^ The current version of the configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The streaming distribution's information.
usdsStreamingDistribution
    :: Functor f
    => (Maybe StreamingDistribution
    -> f (Maybe StreamingDistribution))
    -> UpdateStreamingDistributionResponse
    -> f UpdateStreamingDistributionResponse
usdsStreamingDistribution f x =
    (\y -> x { _usdsStreamingDistribution = y })
       <$> f (_usdsStreamingDistribution x)
{-# INLINE usdsStreamingDistribution #-}

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
usdsETag
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStreamingDistributionResponse
    -> f UpdateStreamingDistributionResponse
usdsETag f x =
    (\y -> x { _usdsETag = y })
       <$> f (_usdsETag x)
{-# INLINE usdsETag #-}

instance AWSRequest UpdateStreamingDistribution where
    type Sv UpdateStreamingDistribution = CloudFront
    type Rs UpdateStreamingDistribution = UpdateStreamingDistributionResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure UpdateStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
