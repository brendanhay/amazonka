{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new streaming distribution.
module Network.AWS.CloudFront
    (
    -- * Request
      CreateStreamingDistribution
    -- ** Request constructor
    , mkCreateStreamingDistribution
    -- ** Request lenses
    , csdStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistributionResponse
    -- ** Response constructor
    , mkCreateStreamingDistributionResponse
    -- ** Response lenses
    , csdrStreamingDistribution
    , csdrLocation
    , csdrETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create a new streaming distribution.
newtype CreateStreamingDistribution = CreateStreamingDistribution
    { _csdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStreamingDistribution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamingDistributionConfig ::@ @StreamingDistributionConfig@
--
mkCreateStreamingDistribution :: StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
                              -> CreateStreamingDistribution
mkCreateStreamingDistribution p1 = CreateStreamingDistribution
    { _csdStreamingDistributionConfig = p1
    }

-- | The streaming distribution's configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution StreamingDistributionConfig
csdStreamingDistributionConfig =
    lens _csdStreamingDistributionConfig
         (\s a -> s { _csdStreamingDistributionConfig = a })

instance ToPath CreateStreamingDistribution

instance ToQuery CreateStreamingDistribution

instance ToHeaders CreateStreamingDistribution

instance ToXML CreateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateStreamingDistributionRequest"

-- | The returned result of the corresponding request.
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse
    { _csdrStreamingDistribution :: Maybe StreamingDistribution
    , _csdrLocation :: !(Maybe Text)
    , _csdrETag :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStreamingDistributionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamingDistribution ::@ @Maybe StreamingDistribution@
--
-- * @Location ::@ @Maybe Text@
--
-- * @ETag ::@ @Maybe Text@
--
mkCreateStreamingDistributionResponse :: CreateStreamingDistributionResponse
mkCreateStreamingDistributionResponse = CreateStreamingDistributionResponse
    { _csdrStreamingDistribution = Nothing
    , _csdrLocation = Nothing
    , _csdrETag = Nothing
    }

-- | The streaming distribution's information.
csdrStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdrStreamingDistribution =
    lens _csdrStreamingDistribution
         (\s a -> s { _csdrStreamingDistribution = a })

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
-- 
csdrLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrLocation = lens _csdrLocation (\s a -> s { _csdrLocation = a })

-- | The current version of the streaming distribution created.
csdrETag :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrETag = lens _csdrETag (\s a -> s { _csdrETag = a })

instance AWSRequest CreateStreamingDistribution where
    type Sv CreateStreamingDistribution = CloudFront
    type Rs CreateStreamingDistribution = CreateStreamingDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CreateStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
