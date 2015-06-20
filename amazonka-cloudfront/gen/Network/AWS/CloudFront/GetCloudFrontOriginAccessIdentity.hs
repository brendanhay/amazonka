{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
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

-- | Get the information about an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
    (
    -- * Request
      GetCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , gcfoaiId

    -- * Response
    , GetCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , gcfoairETag
    , gcfoairCloudFrontOriginAccessIdentity
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCloudFrontOriginAccessIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaiId'
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'{_gcfoaiId :: Text} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentity' smart constructor.
getCloudFrontOriginAccessIdentity :: Text -> GetCloudFrontOriginAccessIdentity
getCloudFrontOriginAccessIdentity pId = GetCloudFrontOriginAccessIdentity'{_gcfoaiId = pId};

-- | The identity\'s id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity Text
gcfoaiId = lens _gcfoaiId (\ s a -> s{_gcfoaiId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetCloudFrontOriginAccessIdentity
         where
        type Sv GetCloudFrontOriginAccessIdentity =
             CloudFront
        type Rs GetCloudFrontOriginAccessIdentity =
             GetCloudFrontOriginAccessIdentityResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentityResponse' <$>
                   (h .#? "ETag") <*>
                     (x .@? "CloudFrontOriginAccessIdentity"))

instance ToHeaders GetCloudFrontOriginAccessIdentity
         where
        toHeaders = const mempty

instance ToPath GetCloudFrontOriginAccessIdentity
         where
        toPath GetCloudFrontOriginAccessIdentity'{..}
          = mconcat
              ["/2014-11-06/origin-access-identity/cloudfront/",
               toText _gcfoaiId]

instance ToQuery GetCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | /See:/ 'getCloudFrontOriginAccessIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoairETag'
--
-- * 'gcfoairCloudFrontOriginAccessIdentity'
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'{_gcfoairETag :: Maybe Text, _gcfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentityResponse' smart constructor.
getCloudFrontOriginAccessIdentityResponse :: GetCloudFrontOriginAccessIdentityResponse
getCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'{_gcfoairETag = Nothing, _gcfoairCloudFrontOriginAccessIdentity = Nothing};

-- | The current version of the origin access identity\'s information. For
-- example: E2QWRUHAPOMQZL.
gcfoairETag :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe Text)
gcfoairETag = lens _gcfoairETag (\ s a -> s{_gcfoairETag = a});

-- | The origin access identity\'s information.
gcfoairCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
gcfoairCloudFrontOriginAccessIdentity = lens _gcfoairCloudFrontOriginAccessIdentity (\ s a -> s{_gcfoairCloudFrontOriginAccessIdentity = a});
