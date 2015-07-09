{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Get the configuration information about a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetStreamingDistributionConfig.html>
module Network.AWS.CloudFront.GetStreamingDistributionConfig
    (
    -- * Request
      GetStreamingDistributionConfig
    -- ** Request constructor
    , getStreamingDistributionConfig
    -- ** Request lenses
    , gsdcId

    -- * Response
    , GetStreamingDistributionConfigResponse
    -- ** Response constructor
    , getStreamingDistributionConfigResponse
    -- ** Response lenses
    , gsdcrStreamingDistributionConfig
    , gsdcrETag
    , gsdcrStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | To request to get a streaming distribution configuration.
--
-- /See:/ 'getStreamingDistributionConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcId'
newtype GetStreamingDistributionConfig = GetStreamingDistributionConfig'
    { _gsdcId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetStreamingDistributionConfig' smart constructor.
getStreamingDistributionConfig :: Text -> GetStreamingDistributionConfig
getStreamingDistributionConfig pId =
    GetStreamingDistributionConfig'
    { _gsdcId = pId
    }

-- | The streaming distribution\'s id.
gsdcId :: Lens' GetStreamingDistributionConfig Text
gsdcId = lens _gsdcId (\ s a -> s{_gsdcId = a});

instance AWSRequest GetStreamingDistributionConfig
         where
        type Sv GetStreamingDistributionConfig = CloudFront
        type Rs GetStreamingDistributionConfig =
             GetStreamingDistributionConfigResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetStreamingDistributionConfigResponse' <$>
                   (parseXML x) <*> (h .#? "ETag") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetStreamingDistributionConfig
         where
        toHeaders = const mempty

instance ToPath GetStreamingDistributionConfig where
        toPath GetStreamingDistributionConfig'{..}
          = mconcat
              ["/2015-04-17/streaming-distribution/",
               toText _gsdcId, "/config"]

instance ToQuery GetStreamingDistributionConfig where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getStreamingDistributionConfigResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcrStreamingDistributionConfig'
--
-- * 'gsdcrETag'
--
-- * 'gsdcrStatus'
data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse'
    { _gsdcrStreamingDistributionConfig :: !(Maybe StreamingDistributionConfig)
    , _gsdcrETag                        :: !(Maybe Text)
    , _gsdcrStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetStreamingDistributionConfigResponse' smart constructor.
getStreamingDistributionConfigResponse :: Int -> GetStreamingDistributionConfigResponse
getStreamingDistributionConfigResponse pStatus =
    GetStreamingDistributionConfigResponse'
    { _gsdcrStreamingDistributionConfig = Nothing
    , _gsdcrETag = Nothing
    , _gsdcrStatus = pStatus
    }

-- | The streaming distribution\'s configuration information.
gsdcrStreamingDistributionConfig :: Lens' GetStreamingDistributionConfigResponse (Maybe StreamingDistributionConfig)
gsdcrStreamingDistributionConfig = lens _gsdcrStreamingDistributionConfig (\ s a -> s{_gsdcrStreamingDistributionConfig = a});

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrETag :: Lens' GetStreamingDistributionConfigResponse (Maybe Text)
gsdcrETag = lens _gsdcrETag (\ s a -> s{_gsdcrETag = a});

-- | FIXME: Undocumented member.
gsdcrStatus :: Lens' GetStreamingDistributionConfigResponse Int
gsdcrStatus = lens _gsdcrStatus (\ s a -> s{_gsdcrStatus = a});
