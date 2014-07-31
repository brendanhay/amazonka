{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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
module Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

data CreateStreamingDistribution = CreateStreamingDistribution
    { _csdrStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    } deriving (Generic)

instance ToPath CreateStreamingDistribution where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery CreateStreamingDistribution

instance ToHeaders CreateStreamingDistribution

instance ToXML CreateStreamingDistributionRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateStreamingDistributionRequest"

instance AWSRequest CreateStreamingDistribution where
    type Sv CreateStreamingDistribution = CloudFront
    type Rs CreateStreamingDistribution = CreateStreamingDistributionResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
            <*> hs ~:? "Location"

data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse
    { _csdsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    , _csdsETag :: Maybe Text
      -- ^ The current version of the streaming distribution created.
    , _csdsLocation :: Maybe Text
      -- ^ The fully qualified URI of the new streaming distribution
      -- resource just created. For example:
      -- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
      -- 
    } deriving (Generic)
