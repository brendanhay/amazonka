{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetDistribution2014_11_06
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistribution2014_11_06.html>
module Network.AWS.CloudFront.GetDistribution2014_11_06
    (
    -- * Request
      GetDistribution2014_11_06
    -- ** Request constructor
    , getDistribution2014_11_06
    -- ** Request lenses
    , gdId

    -- * Response
    , GetDistribution2014_11_06Response
    -- ** Response constructor
    , getDistribution2014_11_06Response
    -- ** Response lenses
    , gdrETag
    , gdrDistribution
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'getDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdId'
newtype GetDistribution2014_11_06 = GetDistribution2014_11_06'{_gdId :: Text} deriving (Eq, Read, Show)

-- | 'GetDistribution2014_11_06' smart constructor.
getDistribution2014_11_06 :: Text -> GetDistribution2014_11_06
getDistribution2014_11_06 pId = GetDistribution2014_11_06'{_gdId = pId};

-- | The distribution\'s id.
gdId :: Lens' GetDistribution2014_11_06 Text
gdId = lens _gdId (\ s a -> s{_gdId = a});

instance AWSRequest GetDistribution2014_11_06 where
        type Sv GetDistribution2014_11_06 = CloudFront
        type Rs GetDistribution2014_11_06 =
             GetDistribution2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetDistribution2014_11_06Response' <$>
                   h .#? "ETag" <*> x .@? "Distribution")

instance ToHeaders GetDistribution2014_11_06 where
        toHeaders = const mempty

instance ToPath GetDistribution2014_11_06 where
        toPath GetDistribution2014_11_06'{..}
          = mconcat ["/2014-11-06/distribution/", toText _gdId]

instance ToQuery GetDistribution2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'getDistribution2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrETag'
--
-- * 'gdrDistribution'
data GetDistribution2014_11_06Response = GetDistribution2014_11_06Response'{_gdrETag :: Maybe Text, _gdrDistribution :: Maybe Distribution} deriving (Eq, Read, Show)

-- | 'GetDistribution2014_11_06Response' smart constructor.
getDistribution2014_11_06Response :: GetDistribution2014_11_06Response
getDistribution2014_11_06Response = GetDistribution2014_11_06Response'{_gdrETag = Nothing, _gdrDistribution = Nothing};

-- | The current version of the distribution\'s information. For example:
-- E2QWRUHAPOMQZL.
gdrETag :: Lens' GetDistribution2014_11_06Response (Maybe Text)
gdrETag = lens _gdrETag (\ s a -> s{_gdrETag = a});

-- | The distribution\'s information.
gdrDistribution :: Lens' GetDistribution2014_11_06Response (Maybe Distribution)
gdrDistribution = lens _gdrDistribution (\ s a -> s{_gdrDistribution = a});
