{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution2014_11_06
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Create a new streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateStreamingDistribution2014_11_06.html>
module Network.AWS.CloudFront.CreateStreamingDistribution2014_11_06
    (
    -- * Request
      CreateStreamingDistribution2014_11_06
    -- ** Request constructor
    , createStreamingDistribution2014_11_06
    -- ** Request lenses
    , csdStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistribution2014_11_06Response
    -- ** Response constructor
    , createStreamingDistribution2014_11_06Response
    -- ** Response lenses
    , csdrETag
    , csdrLocation
    , csdrStreamingDistribution
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'createStreamingDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdStreamingDistributionConfig'
newtype CreateStreamingDistribution2014_11_06 = CreateStreamingDistribution2014_11_06'{_csdStreamingDistributionConfig :: StreamingDistributionConfig} deriving (Eq, Read, Show)

-- | 'CreateStreamingDistribution2014_11_06' smart constructor.
createStreamingDistribution2014_11_06 :: StreamingDistributionConfig -> CreateStreamingDistribution2014_11_06
createStreamingDistribution2014_11_06 pStreamingDistributionConfig = CreateStreamingDistribution2014_11_06'{_csdStreamingDistributionConfig = pStreamingDistributionConfig};

-- | The streaming distribution\'s configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution2014_11_06 StreamingDistributionConfig
csdStreamingDistributionConfig = lens _csdStreamingDistributionConfig (\ s a -> s{_csdStreamingDistributionConfig = a});

instance AWSRequest
         CreateStreamingDistribution2014_11_06 where
        type Sv CreateStreamingDistribution2014_11_06 =
             CloudFront
        type Rs CreateStreamingDistribution2014_11_06 =
             CreateStreamingDistribution2014_11_06Response
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateStreamingDistribution2014_11_06Response' <$>
                   h .#? "ETag" <*> h .#? "Location" <*>
                     x .@? "StreamingDistribution")

instance ToElement
         CreateStreamingDistribution2014_11_06 where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}StreamingDistributionConfig"
              .
              _csdStreamingDistributionConfig

instance ToHeaders
         CreateStreamingDistribution2014_11_06 where
        toHeaders = const mempty

instance ToPath CreateStreamingDistribution2014_11_06
         where
        toPath = const "/2014-11-06/streaming-distribution"

instance ToQuery
         CreateStreamingDistribution2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'createStreamingDistribution2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdrETag'
--
-- * 'csdrLocation'
--
-- * 'csdrStreamingDistribution'
data CreateStreamingDistribution2014_11_06Response = CreateStreamingDistribution2014_11_06Response'{_csdrETag :: Maybe Text, _csdrLocation :: Maybe Text, _csdrStreamingDistribution :: Maybe StreamingDistribution} deriving (Eq, Read, Show)

-- | 'CreateStreamingDistribution2014_11_06Response' smart constructor.
createStreamingDistribution2014_11_06Response :: CreateStreamingDistribution2014_11_06Response
createStreamingDistribution2014_11_06Response = CreateStreamingDistribution2014_11_06Response'{_csdrETag = Nothing, _csdrLocation = Nothing, _csdrStreamingDistribution = Nothing};

-- | The current version of the streaming distribution created.
csdrETag :: Lens' CreateStreamingDistribution2014_11_06Response (Maybe Text)
csdrETag = lens _csdrETag (\ s a -> s{_csdrETag = a});

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/streaming-distribution\/EGTXBD79H29TRA8.
csdrLocation :: Lens' CreateStreamingDistribution2014_11_06Response (Maybe Text)
csdrLocation = lens _csdrLocation (\ s a -> s{_csdrLocation = a});

-- | The streaming distribution\'s information.
csdrStreamingDistribution :: Lens' CreateStreamingDistribution2014_11_06Response (Maybe StreamingDistribution)
csdrStreamingDistribution = lens _csdrStreamingDistribution (\ s a -> s{_csdrStreamingDistribution = a});
