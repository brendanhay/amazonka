{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetDistributionConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a distribution.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistributionConfig.html AWS API Reference> for GetDistributionConfig.
module Network.AWS.CloudFront.GetDistributionConfig
    (
    -- * Creating a Request
      GetDistributionConfig
    , getDistributionConfig
    -- * Request Lenses
    , gdcId

    -- * Destructuring the Response
    , GetDistributionConfigResponse
    , getDistributionConfigResponse
    -- * Response Lenses
    , gdcrsETag
    , gdcrsDistributionConfig
    , gdcrsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get a distribution configuration.
--
-- /See:/ 'getDistributionConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcId'
newtype GetDistributionConfig = GetDistributionConfig'
    { _gdcId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDistributionConfig' smart constructor.
getDistributionConfig :: Text -> GetDistributionConfig
getDistributionConfig pId_ =
    GetDistributionConfig'
    { _gdcId = pId_
    }

-- | The distribution\'s id.
gdcId :: Lens' GetDistributionConfig Text
gdcId = lens _gdcId (\ s a -> s{_gdcId = a});

instance AWSRequest GetDistributionConfig where
        type Sv GetDistributionConfig = CloudFront
        type Rs GetDistributionConfig =
             GetDistributionConfigResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetDistributionConfigResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetDistributionConfig where
        toHeaders = const mempty

instance ToPath GetDistributionConfig where
        toPath GetDistributionConfig'{..}
          = mconcat
              ["/2015-04-17/distribution/", toBS _gdcId, "/config"]

instance ToQuery GetDistributionConfig where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getDistributionConfigResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcrsETag'
--
-- * 'gdcrsDistributionConfig'
--
-- * 'gdcrsStatus'
data GetDistributionConfigResponse = GetDistributionConfigResponse'
    { _gdcrsETag               :: !(Maybe Text)
    , _gdcrsDistributionConfig :: !(Maybe DistributionConfig)
    , _gdcrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDistributionConfigResponse' smart constructor.
getDistributionConfigResponse :: Int -> GetDistributionConfigResponse
getDistributionConfigResponse pStatus_ =
    GetDistributionConfigResponse'
    { _gdcrsETag = Nothing
    , _gdcrsDistributionConfig = Nothing
    , _gdcrsStatus = pStatus_
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gdcrsETag :: Lens' GetDistributionConfigResponse (Maybe Text)
gdcrsETag = lens _gdcrsETag (\ s a -> s{_gdcrsETag = a});

-- | The distribution\'s configuration information.
gdcrsDistributionConfig :: Lens' GetDistributionConfigResponse (Maybe DistributionConfig)
gdcrsDistributionConfig = lens _gdcrsDistributionConfig (\ s a -> s{_gdcrsDistributionConfig = a});

-- | Undocumented member.
gdcrsStatus :: Lens' GetDistributionConfigResponse Int
gdcrsStatus = lens _gdcrsStatus (\ s a -> s{_gdcrsStatus = a});
