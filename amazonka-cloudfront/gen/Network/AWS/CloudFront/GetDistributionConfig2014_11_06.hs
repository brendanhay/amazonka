{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetDistributionConfig2014_11_06
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

-- | Get the configuration information about a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistributionConfig2014_11_06.html>
module Network.AWS.CloudFront.GetDistributionConfig2014_11_06
    (
    -- * Request
      GetDistributionConfig2014_11_06
    -- ** Request constructor
    , getDistributionConfig2014_11_06
    -- ** Request lenses
    , gdcId

    -- * Response
    , GetDistributionConfig2014_11_06Response
    -- ** Response constructor
    , getDistributionConfig2014_11_06Response
    -- ** Response lenses
    , gdcrETag
    , gdcrDistributionConfig
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'getDistributionConfig2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcId'
newtype GetDistributionConfig2014_11_06 = GetDistributionConfig2014_11_06'{_gdcId :: Text} deriving (Eq, Read, Show)

-- | 'GetDistributionConfig2014_11_06' smart constructor.
getDistributionConfig2014_11_06 :: Text -> GetDistributionConfig2014_11_06
getDistributionConfig2014_11_06 pId = GetDistributionConfig2014_11_06'{_gdcId = pId};

-- | The distribution\'s id.
gdcId :: Lens' GetDistributionConfig2014_11_06 Text
gdcId = lens _gdcId (\ s a -> s{_gdcId = a});

instance AWSRequest GetDistributionConfig2014_11_06
         where
        type Sv GetDistributionConfig2014_11_06 = CloudFront
        type Rs GetDistributionConfig2014_11_06 =
             GetDistributionConfig2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetDistributionConfig2014_11_06Response' <$>
                   h .#? "ETag" <*> x .@? "DistributionConfig")

instance ToHeaders GetDistributionConfig2014_11_06
         where
        toHeaders = const mempty

instance ToPath GetDistributionConfig2014_11_06 where
        toPath GetDistributionConfig2014_11_06'{..}
          = mconcat
              ["/2014-11-06/distribution/", toText _gdcId,
               "/config"]

instance ToQuery GetDistributionConfig2014_11_06
         where
        toQuery = const mempty

-- | /See:/ 'getDistributionConfig2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcrETag'
--
-- * 'gdcrDistributionConfig'
data GetDistributionConfig2014_11_06Response = GetDistributionConfig2014_11_06Response'{_gdcrETag :: Maybe Text, _gdcrDistributionConfig :: Maybe DistributionConfig} deriving (Eq, Read, Show)

-- | 'GetDistributionConfig2014_11_06Response' smart constructor.
getDistributionConfig2014_11_06Response :: GetDistributionConfig2014_11_06Response
getDistributionConfig2014_11_06Response = GetDistributionConfig2014_11_06Response'{_gdcrETag = Nothing, _gdcrDistributionConfig = Nothing};

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gdcrETag :: Lens' GetDistributionConfig2014_11_06Response (Maybe Text)
gdcrETag = lens _gdcrETag (\ s a -> s{_gdcrETag = a});

-- | The distribution\'s configuration information.
gdcrDistributionConfig :: Lens' GetDistributionConfig2014_11_06Response (Maybe DistributionConfig)
gdcrDistributionConfig = lens _gdcrDistributionConfig (\ s a -> s{_gdcrDistributionConfig = a});
