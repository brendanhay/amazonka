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
{-# INLINE createStreamingDistribution #-}

data CreateStreamingDistribution = CreateStreamingDistribution
    { _csdrStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    } deriving (Show, Generic)

-- | The streaming distribution's configuration information.
csdrStreamingDistributionConfig :: Lens' CreateStreamingDistribution (StreamingDistributionConfig)
csdrStreamingDistributionConfig f x =
    f (_csdrStreamingDistributionConfig x)
        <&> \y -> x { _csdrStreamingDistributionConfig = y }
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
csdsStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdsStreamingDistribution f x =
    f (_csdsStreamingDistribution x)
        <&> \y -> x { _csdsStreamingDistribution = y }
{-# INLINE csdsStreamingDistribution #-}

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
-- 
csdsLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdsLocation f x =
    f (_csdsLocation x)
        <&> \y -> x { _csdsLocation = y }
{-# INLINE csdsLocation #-}

-- | The current version of the streaming distribution created.
csdsETag :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdsETag f x =
    f (_csdsETag x)
        <&> \y -> x { _csdsETag = y }
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
