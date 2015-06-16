{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetStreamingDistributionConfig2014_11_06
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

-- | Get the configuration information about a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetStreamingDistributionConfig2014_11_06.html>
module Network.AWS.CloudFront.GetStreamingDistributionConfig2014_11_06
    (
    -- * Request
      GetStreamingDistributionConfig2014_11_06
    -- ** Request constructor
    , getStreamingDistributionConfig2014_11_06
    -- ** Request lenses
    , gsdcId

    -- * Response
    , GetStreamingDistributionConfig2014_11_06Response
    -- ** Response constructor
    , getStreamingDistributionConfig2014_11_06Response
    -- ** Response lenses
    , gsdcrStreamingDistributionConfig
    , gsdcrETag
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'getStreamingDistributionConfig2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcId'
newtype GetStreamingDistributionConfig2014_11_06 = GetStreamingDistributionConfig2014_11_06'{_gsdcId :: Text} deriving (Eq, Read, Show)

-- | 'GetStreamingDistributionConfig2014_11_06' smart constructor.
getStreamingDistributionConfig2014_11_06 :: Text -> GetStreamingDistributionConfig2014_11_06
getStreamingDistributionConfig2014_11_06 pId = GetStreamingDistributionConfig2014_11_06'{_gsdcId = pId};

-- | The streaming distribution\'s id.
gsdcId :: Lens' GetStreamingDistributionConfig2014_11_06 Text
gsdcId = lens _gsdcId (\ s a -> s{_gsdcId = a});

instance AWSRequest
         GetStreamingDistributionConfig2014_11_06 where
        type Sv GetStreamingDistributionConfig2014_11_06 =
             CloudFront
        type Rs GetStreamingDistributionConfig2014_11_06 =
             GetStreamingDistributionConfig2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetStreamingDistributionConfig2014_11_06Response' <$>
                   (x .@? "StreamingDistributionConfig") <*>
                     (h .#? "ETag"))

instance ToHeaders
         GetStreamingDistributionConfig2014_11_06 where
        toHeaders = const mempty

instance ToPath
         GetStreamingDistributionConfig2014_11_06 where
        toPath GetStreamingDistributionConfig2014_11_06'{..}
          = mconcat
              ["/2014-11-06/streaming-distribution/",
               toText _gsdcId, "/config"]

instance ToQuery
         GetStreamingDistributionConfig2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'getStreamingDistributionConfig2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcrStreamingDistributionConfig'
--
-- * 'gsdcrETag'
data GetStreamingDistributionConfig2014_11_06Response = GetStreamingDistributionConfig2014_11_06Response'{_gsdcrStreamingDistributionConfig :: Maybe StreamingDistributionConfig, _gsdcrETag :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetStreamingDistributionConfig2014_11_06Response' smart constructor.
getStreamingDistributionConfig2014_11_06Response :: GetStreamingDistributionConfig2014_11_06Response
getStreamingDistributionConfig2014_11_06Response = GetStreamingDistributionConfig2014_11_06Response'{_gsdcrStreamingDistributionConfig = Nothing, _gsdcrETag = Nothing};

-- | The streaming distribution\'s configuration information.
gsdcrStreamingDistributionConfig :: Lens' GetStreamingDistributionConfig2014_11_06Response (Maybe StreamingDistributionConfig)
gsdcrStreamingDistributionConfig = lens _gsdcrStreamingDistributionConfig (\ s a -> s{_gsdcrStreamingDistributionConfig = a});

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrETag :: Lens' GetStreamingDistributionConfig2014_11_06Response (Maybe Text)
gsdcrETag = lens _gsdcrETag (\ s a -> s{_gsdcrETag = a});
