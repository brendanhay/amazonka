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
    , mkCreateStreamingDistribution
    -- ** Request lenses
    , csdStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistributionResponse
    -- ** Response lenses
    , csdrsStreamingDistribution
    , csdrsLocation
    , csdrsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create a new streaming distribution.
newtype CreateStreamingDistribution = CreateStreamingDistribution
    { _csdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStreamingDistribution' request.
mkCreateStreamingDistribution :: StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
                              -> CreateStreamingDistribution
mkCreateStreamingDistribution p1 = CreateStreamingDistribution
    { _csdStreamingDistributionConfig = p1
    }
{-# INLINE mkCreateStreamingDistribution #-}

-- | The streaming distribution's configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution StreamingDistributionConfig
csdStreamingDistributionConfig =
    lens _csdStreamingDistributionConfig
         (\s a -> s { _csdStreamingDistributionConfig = a })
{-# INLINE csdStreamingDistributionConfig #-}

instance ToPath CreateStreamingDistribution where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery CreateStreamingDistribution

instance ToHeaders CreateStreamingDistribution

instance ToXML CreateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateStreamingDistributionRequest"

-- | The returned result of the corresponding request.
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse
    { _csdrsStreamingDistribution :: Maybe StreamingDistribution
    , _csdrsLocation :: Maybe Text
    , _csdrsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The streaming distribution's information.
csdrsStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdrsStreamingDistribution =
    lens _csdrsStreamingDistribution
         (\s a -> s { _csdrsStreamingDistribution = a })
{-# INLINE csdrsStreamingDistribution #-}

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
-- 
csdrsLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrsLocation = lens _csdrsLocation (\s a -> s { _csdrsLocation = a })
{-# INLINE csdrsLocation #-}

-- | The current version of the streaming distribution created.
csdrsETag :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrsETag = lens _csdrsETag (\s a -> s { _csdrsETag = a })
{-# INLINE csdrsETag #-}

instance AWSRequest CreateStreamingDistribution where
    type Sv CreateStreamingDistribution = CloudFront
    type Rs CreateStreamingDistribution = CreateStreamingDistributionResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
