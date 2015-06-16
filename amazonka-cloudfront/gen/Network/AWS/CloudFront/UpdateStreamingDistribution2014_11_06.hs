{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution2014_11_06
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

-- | Update a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateStreamingDistribution2014_11_06.html>
module Network.AWS.CloudFront.UpdateStreamingDistribution2014_11_06
    (
    -- * Request
      UpdateStreamingDistribution2014_11_06
    -- ** Request constructor
    , updateStreamingDistribution2014_11_06
    -- ** Request lenses
    , usdIfMatch
    , usdStreamingDistributionConfig
    , usdId

    -- * Response
    , UpdateStreamingDistribution2014_11_06Response
    -- ** Response constructor
    , updateStreamingDistribution2014_11_06Response
    -- ** Response lenses
    , usdrETag
    , usdrStreamingDistribution
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'updateStreamingDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdIfMatch'
--
-- * 'usdStreamingDistributionConfig'
--
-- * 'usdId'
data UpdateStreamingDistribution2014_11_06 = UpdateStreamingDistribution2014_11_06'{_usdIfMatch :: Maybe Text, _usdStreamingDistributionConfig :: StreamingDistributionConfig, _usdId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateStreamingDistribution2014_11_06' smart constructor.
updateStreamingDistribution2014_11_06 :: StreamingDistributionConfig -> Text -> UpdateStreamingDistribution2014_11_06
updateStreamingDistribution2014_11_06 pStreamingDistributionConfig pId = UpdateStreamingDistribution2014_11_06'{_usdIfMatch = Nothing, _usdStreamingDistributionConfig = pStreamingDistributionConfig, _usdId = pId};

-- | The value of the ETag header you received when retrieving the streaming
-- distribution\'s configuration. For example: E2QWRUHAPOMQZL.
usdIfMatch :: Lens' UpdateStreamingDistribution2014_11_06 (Maybe Text)
usdIfMatch = lens _usdIfMatch (\ s a -> s{_usdIfMatch = a});

-- | The streaming distribution\'s configuration information.
usdStreamingDistributionConfig :: Lens' UpdateStreamingDistribution2014_11_06 StreamingDistributionConfig
usdStreamingDistributionConfig = lens _usdStreamingDistributionConfig (\ s a -> s{_usdStreamingDistributionConfig = a});

-- | The streaming distribution\'s id.
usdId :: Lens' UpdateStreamingDistribution2014_11_06 Text
usdId = lens _usdId (\ s a -> s{_usdId = a});

instance AWSRequest
         UpdateStreamingDistribution2014_11_06 where
        type Sv UpdateStreamingDistribution2014_11_06 =
             CloudFront
        type Rs UpdateStreamingDistribution2014_11_06 =
             UpdateStreamingDistribution2014_11_06Response
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateStreamingDistribution2014_11_06Response' <$>
                   (h .#? "ETag") <*> (x .@? "StreamingDistribution"))

instance ToElement
         UpdateStreamingDistribution2014_11_06 where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}StreamingDistributionConfig"
              .
              _usdStreamingDistributionConfig

instance ToHeaders
         UpdateStreamingDistribution2014_11_06 where
        toHeaders UpdateStreamingDistribution2014_11_06'{..}
          = mconcat ["If-Match" =# _usdIfMatch]

instance ToPath UpdateStreamingDistribution2014_11_06
         where
        toPath UpdateStreamingDistribution2014_11_06'{..}
          = mconcat
              ["/2014-11-06/streaming-distribution/",
               toText _usdId, "/config"]

instance ToQuery
         UpdateStreamingDistribution2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'updateStreamingDistribution2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdrETag'
--
-- * 'usdrStreamingDistribution'
data UpdateStreamingDistribution2014_11_06Response = UpdateStreamingDistribution2014_11_06Response'{_usdrETag :: Maybe Text, _usdrStreamingDistribution :: Maybe StreamingDistribution} deriving (Eq, Read, Show)

-- | 'UpdateStreamingDistribution2014_11_06Response' smart constructor.
updateStreamingDistribution2014_11_06Response :: UpdateStreamingDistribution2014_11_06Response
updateStreamingDistribution2014_11_06Response = UpdateStreamingDistribution2014_11_06Response'{_usdrETag = Nothing, _usdrStreamingDistribution = Nothing};

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
usdrETag :: Lens' UpdateStreamingDistribution2014_11_06Response (Maybe Text)
usdrETag = lens _usdrETag (\ s a -> s{_usdrETag = a});

-- | The streaming distribution\'s information.
usdrStreamingDistribution :: Lens' UpdateStreamingDistribution2014_11_06Response (Maybe StreamingDistribution)
usdrStreamingDistribution = lens _usdrStreamingDistribution (\ s a -> s{_usdrStreamingDistribution = a});
