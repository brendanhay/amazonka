{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.GetDistributionConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Get the configuration information about a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistributionConfig.html>
module Network.AWS.CloudFront.GetDistributionConfig
    (
    -- * Request
      GetDistributionConfig
    -- ** Request constructor
    , getDistributionConfig
    -- ** Request lenses
    , gdcId

    -- * Response
    , GetDistributionConfigResponse
    -- ** Response constructor
    , getDistributionConfigResponse
    -- ** Response lenses
    , gdcrETag
    , gdcrDistributionConfig
    , gdcrStatus
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
    } deriving (Eq,Read,Show)

-- | 'GetDistributionConfig' smart constructor.
getDistributionConfig :: Text -> GetDistributionConfig
getDistributionConfig pId =
    GetDistributionConfig'
    { _gdcId = pId
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
                   (h .#? "ETag") <*> (x .@? "DistributionConfig") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetDistributionConfig where
        toHeaders = const mempty

instance ToPath GetDistributionConfig where
        toPath GetDistributionConfig'{..}
          = mconcat
              ["/2015-04-17/distribution/", toText _gdcId,
               "/config"]

instance ToQuery GetDistributionConfig where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getDistributionConfigResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcrETag'
--
-- * 'gdcrDistributionConfig'
--
-- * 'gdcrStatus'
data GetDistributionConfigResponse = GetDistributionConfigResponse'
    { _gdcrETag               :: !(Maybe Text)
    , _gdcrDistributionConfig :: !(Maybe DistributionConfig)
    , _gdcrStatus             :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetDistributionConfigResponse' smart constructor.
getDistributionConfigResponse :: Int -> GetDistributionConfigResponse
getDistributionConfigResponse pStatus =
    GetDistributionConfigResponse'
    { _gdcrETag = Nothing
    , _gdcrDistributionConfig = Nothing
    , _gdcrStatus = pStatus
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gdcrETag :: Lens' GetDistributionConfigResponse (Maybe Text)
gdcrETag = lens _gdcrETag (\ s a -> s{_gdcrETag = a});

-- | The distribution\'s configuration information.
gdcrDistributionConfig :: Lens' GetDistributionConfigResponse (Maybe DistributionConfig)
gdcrDistributionConfig = lens _gdcrDistributionConfig (\ s a -> s{_gdcrDistributionConfig = a});

-- | FIXME: Undocumented member.
gdcrStatus :: Lens' GetDistributionConfigResponse Int
gdcrStatus = lens _gdcrStatus (\ s a -> s{_gdcrStatus = a});
