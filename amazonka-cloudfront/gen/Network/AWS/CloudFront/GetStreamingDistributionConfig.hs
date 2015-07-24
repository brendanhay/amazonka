{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a streaming distribution.
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
    , gsdcrsStreamingDistributionConfig
    , gsdcrsETag
    , gsdcrsStatus
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
getStreamingDistributionConfig pId_ =
    GetStreamingDistributionConfig'
    { _gsdcId = pId_
    }

-- | The streaming distribution\'s id.
gsdcId :: Lens' GetStreamingDistributionConfig Text
gsdcId = lens _gsdcId (\ s a -> s{_gsdcId = a});

instance AWSRequest GetStreamingDistributionConfig
         where
        type Sv GetStreamingDistributionConfig = CloudFront
        type Rs GetStreamingDistributionConfig =
             GetStreamingDistributionConfigResponse
        request = get "GetStreamingDistributionConfig"
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
-- * 'gsdcrsStreamingDistributionConfig'
--
-- * 'gsdcrsETag'
--
-- * 'gsdcrsStatus'
data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse'
    { _gsdcrsStreamingDistributionConfig :: !(Maybe StreamingDistributionConfig)
    , _gsdcrsETag                        :: !(Maybe Text)
    , _gsdcrsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetStreamingDistributionConfigResponse' smart constructor.
getStreamingDistributionConfigResponse :: Int -> GetStreamingDistributionConfigResponse
getStreamingDistributionConfigResponse pStatus_ =
    GetStreamingDistributionConfigResponse'
    { _gsdcrsStreamingDistributionConfig = Nothing
    , _gsdcrsETag = Nothing
    , _gsdcrsStatus = pStatus_
    }

-- | The streaming distribution\'s configuration information.
gsdcrsStreamingDistributionConfig :: Lens' GetStreamingDistributionConfigResponse (Maybe StreamingDistributionConfig)
gsdcrsStreamingDistributionConfig = lens _gsdcrsStreamingDistributionConfig (\ s a -> s{_gsdcrsStreamingDistributionConfig = a});

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrsETag :: Lens' GetStreamingDistributionConfigResponse (Maybe Text)
gsdcrsETag = lens _gsdcrsETag (\ s a -> s{_gsdcrsETag = a});

-- | FIXME: Undocumented member.
gsdcrsStatus :: Lens' GetStreamingDistributionConfigResponse Int
gsdcrsStatus = lens _gsdcrsStatus (\ s a -> s{_gsdcrsStatus = a});
