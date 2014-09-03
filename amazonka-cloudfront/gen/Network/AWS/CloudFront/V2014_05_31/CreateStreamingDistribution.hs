{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new streaming distribution.
module Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution
    (
    -- * Request
      CreateStreamingDistribution
    -- ** Request constructor
    , createStreamingDistribution
    -- ** Request lenses
    , csdrStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistributionResponse
    -- ** Response lenses
    , csdsStreamingDistribution
    , csdsLocation
    , csdsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateStreamingDistribution' request.
createStreamingDistribution :: StreamingDistributionConfig -- ^ 'csdrStreamingDistributionConfig'
                            -> CreateStreamingDistribution
createStreamingDistribution p1 = CreateStreamingDistribution
    { _csdrStreamingDistributionConfig = p1
    }

data CreateStreamingDistribution = CreateStreamingDistribution
    { _csdrStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    } deriving (Show, Generic)

-- | The streaming distribution's configuration information.
csdrStreamingDistributionConfig
    :: Functor f
    => (StreamingDistributionConfig
    -> f (StreamingDistributionConfig))
    -> CreateStreamingDistribution
    -> f CreateStreamingDistribution
csdrStreamingDistributionConfig f x =
    (\y -> x { _csdrStreamingDistributionConfig = y })
       <$> f (_csdrStreamingDistributionConfig x)
{-# INLINE csdrStreamingDistributionConfig #-}

instance ToPath CreateStreamingDistribution where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery CreateStreamingDistribution

instance ToHeaders CreateStreamingDistribution

instance ToXML CreateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateStreamingDistributionRequest"

data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse
    { _csdsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    , _csdsLocation :: Maybe Text
      -- ^ The fully qualified URI of the new streaming distribution
      -- resource just created. For example:
      -- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
      -- 
    , _csdsETag :: Maybe Text
      -- ^ The current version of the streaming distribution created.
    } deriving (Show, Generic)

-- | The streaming distribution's information.
csdsStreamingDistribution
    :: Functor f
    => (Maybe StreamingDistribution
    -> f (Maybe StreamingDistribution))
    -> CreateStreamingDistributionResponse
    -> f CreateStreamingDistributionResponse
csdsStreamingDistribution f x =
    (\y -> x { _csdsStreamingDistribution = y })
       <$> f (_csdsStreamingDistribution x)
{-# INLINE csdsStreamingDistribution #-}

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
-- 
csdsLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateStreamingDistributionResponse
    -> f CreateStreamingDistributionResponse
csdsLocation f x =
    (\y -> x { _csdsLocation = y })
       <$> f (_csdsLocation x)
{-# INLINE csdsLocation #-}

-- | The current version of the streaming distribution created.
csdsETag
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateStreamingDistributionResponse
    -> f CreateStreamingDistributionResponse
csdsETag f x =
    (\y -> x { _csdsETag = y })
       <$> f (_csdsETag x)
{-# INLINE csdsETag #-}

instance AWSRequest CreateStreamingDistribution where
    type Sv CreateStreamingDistribution = CloudFront
    type Rs CreateStreamingDistribution = CreateStreamingDistributionResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
