{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetDistribution
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

-- | Get the information about a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistribution.html>
module Network.AWS.CloudFront.GetDistribution
    (
    -- * Request
      GetDistribution
    -- ** Request constructor
    , getDistribution
    -- ** Request lenses
    , gdId

    -- * Response
    , GetDistributionResponse
    -- ** Response constructor
    , getDistributionResponse
    -- ** Response lenses
    , gdrETag
    , gdrDistribution
    , gdrStatusCode
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to get a distribution\'s information.
--
-- /See:/ 'getDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdId'
newtype GetDistribution = GetDistribution'{_gdId :: Text} deriving (Eq, Read, Show)

-- | 'GetDistribution' smart constructor.
getDistribution :: Text -> GetDistribution
getDistribution pId = GetDistribution'{_gdId = pId};

-- | The distribution\'s id.
gdId :: Lens' GetDistribution Text
gdId = lens _gdId (\ s a -> s{_gdId = a});

instance AWSRequest GetDistribution where
        type Sv GetDistribution = CloudFront
        type Rs GetDistribution = GetDistributionResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetDistributionResponse' <$>
                   (h .#? "ETag") <*> (x .@? "Distribution") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetDistribution where
        toHeaders = const mempty

instance ToPath GetDistribution where
        toPath GetDistribution'{..}
          = mconcat ["/2014-11-06/distribution/", toText _gdId]

instance ToQuery GetDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrETag'
--
-- * 'gdrDistribution'
--
-- * 'gdrStatusCode'
data GetDistributionResponse = GetDistributionResponse'{_gdrETag :: Maybe Text, _gdrDistribution :: Maybe Distribution, _gdrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetDistributionResponse' smart constructor.
getDistributionResponse :: Int -> GetDistributionResponse
getDistributionResponse pStatusCode = GetDistributionResponse'{_gdrETag = Nothing, _gdrDistribution = Nothing, _gdrStatusCode = pStatusCode};

-- | The current version of the distribution\'s information. For example:
-- E2QWRUHAPOMQZL.
gdrETag :: Lens' GetDistributionResponse (Maybe Text)
gdrETag = lens _gdrETag (\ s a -> s{_gdrETag = a});

-- | The distribution\'s information.
gdrDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrDistribution = lens _gdrDistribution (\ s a -> s{_gdrDistribution = a});

-- | FIXME: Undocumented member.
gdrStatusCode :: Lens' GetDistributionResponse Int
gdrStatusCode = lens _gdrStatusCode (\ s a -> s{_gdrStatusCode = a});
