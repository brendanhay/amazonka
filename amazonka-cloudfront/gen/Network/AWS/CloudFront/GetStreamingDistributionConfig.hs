{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a streaming distribution.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetStreamingDistributionConfig.html AWS API Reference> for GetStreamingDistributionConfig.
module Network.AWS.CloudFront.GetStreamingDistributionConfig
    (
    -- * Creating a Request
      getStreamingDistributionConfig
    , GetStreamingDistributionConfig
    -- * Request Lenses
    , gsdcId

    -- * Destructuring the Response
    , getStreamingDistributionConfigResponse
    , GetStreamingDistributionConfigResponse
    -- * Response Lenses
    , gsdcrsStreamingDistributionConfig
    , gsdcrsETag
    , gsdcrsResponseStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | To request to get a streaming distribution configuration.
--
-- /See:/ 'getStreamingDistributionConfig' smart constructor.
newtype GetStreamingDistributionConfig = GetStreamingDistributionConfig'
    { _gsdcId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetStreamingDistributionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdcId'
getStreamingDistributionConfig
    :: Text -- ^ 'gsdcId'
    -> GetStreamingDistributionConfig
getStreamingDistributionConfig pId_ =
    GetStreamingDistributionConfig'
    { _gsdcId = pId_
    }

-- | The streaming distribution\'s id.
gsdcId :: Lens' GetStreamingDistributionConfig Text
gsdcId = lens _gsdcId (\ s a -> s{_gsdcId = a});

instance AWSRequest GetStreamingDistributionConfig
         where
        type Rs GetStreamingDistributionConfig =
             GetStreamingDistributionConfigResponse
        request = get cloudFront
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
              ["/2015-07-27/streaming-distribution/", toBS _gsdcId,
               "/config"]

instance ToQuery GetStreamingDistributionConfig where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getStreamingDistributionConfigResponse' smart constructor.
data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse'
    { _gsdcrsStreamingDistributionConfig :: !(Maybe StreamingDistributionConfig)
    , _gsdcrsETag                        :: !(Maybe Text)
    , _gsdcrsResponseStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetStreamingDistributionConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdcrsStreamingDistributionConfig'
--
-- * 'gsdcrsETag'
--
-- * 'gsdcrsResponseStatus'
getStreamingDistributionConfigResponse
    :: Int -- ^ 'gsdcrsResponseStatus'
    -> GetStreamingDistributionConfigResponse
getStreamingDistributionConfigResponse pResponseStatus_ =
    GetStreamingDistributionConfigResponse'
    { _gsdcrsStreamingDistributionConfig = Nothing
    , _gsdcrsETag = Nothing
    , _gsdcrsResponseStatus = pResponseStatus_
    }

-- | The streaming distribution\'s configuration information.
gsdcrsStreamingDistributionConfig :: Lens' GetStreamingDistributionConfigResponse (Maybe StreamingDistributionConfig)
gsdcrsStreamingDistributionConfig = lens _gsdcrsStreamingDistributionConfig (\ s a -> s{_gsdcrsStreamingDistributionConfig = a});

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrsETag :: Lens' GetStreamingDistributionConfigResponse (Maybe Text)
gsdcrsETag = lens _gsdcrsETag (\ s a -> s{_gsdcrsETag = a});

-- | The response status code.
gsdcrsResponseStatus :: Lens' GetStreamingDistributionConfigResponse Int
gsdcrsResponseStatus = lens _gsdcrsResponseStatus (\ s a -> s{_gsdcrsResponseStatus = a});
