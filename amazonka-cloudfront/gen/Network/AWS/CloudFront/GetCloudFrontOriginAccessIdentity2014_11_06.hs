{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity2014_11_06
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentity2014_11_06.html>
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity2014_11_06
    (
    -- * Request
      GetCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request lenses
    , gcfoaiId

    -- * Response
    , GetCloudFrontOriginAccessIdentity2014_11_06Response
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentity2014_11_06Response
    -- ** Response lenses
    , gcfoairETag
    , gcfoairCloudFrontOriginAccessIdentity
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'getCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaiId'
newtype GetCloudFrontOriginAccessIdentity2014_11_06 = GetCloudFrontOriginAccessIdentity2014_11_06'{_gcfoaiId :: Text} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
getCloudFrontOriginAccessIdentity2014_11_06 :: Text -> GetCloudFrontOriginAccessIdentity2014_11_06
getCloudFrontOriginAccessIdentity2014_11_06 pId = GetCloudFrontOriginAccessIdentity2014_11_06'{_gcfoaiId = pId};

-- | The identity\'s id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity2014_11_06 Text
gcfoaiId = lens _gcfoaiId (\ s a -> s{_gcfoaiId = a});

instance AWSRequest
         GetCloudFrontOriginAccessIdentity2014_11_06 where
        type Sv GetCloudFrontOriginAccessIdentity2014_11_06 =
             CloudFront
        type Rs GetCloudFrontOriginAccessIdentity2014_11_06 =
             GetCloudFrontOriginAccessIdentity2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentity2014_11_06Response'
                   <$>
                   h .#? "ETag" <*>
                     x .@? "CloudFrontOriginAccessIdentity")

instance ToHeaders
         GetCloudFrontOriginAccessIdentity2014_11_06 where
        toHeaders = const mempty

instance ToPath
         GetCloudFrontOriginAccessIdentity2014_11_06 where
        toPath
          GetCloudFrontOriginAccessIdentity2014_11_06'{..}
          = mconcat
              ["/2014-11-06/origin-access-identity/cloudfront/",
               toText _gcfoaiId]

instance ToQuery
         GetCloudFrontOriginAccessIdentity2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'getCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoairETag'
--
-- * 'gcfoairCloudFrontOriginAccessIdentity'
data GetCloudFrontOriginAccessIdentity2014_11_06Response = GetCloudFrontOriginAccessIdentity2014_11_06Response'{_gcfoairETag :: Maybe Text, _gcfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
getCloudFrontOriginAccessIdentity2014_11_06Response :: GetCloudFrontOriginAccessIdentity2014_11_06Response
getCloudFrontOriginAccessIdentity2014_11_06Response = GetCloudFrontOriginAccessIdentity2014_11_06Response'{_gcfoairETag = Nothing, _gcfoairCloudFrontOriginAccessIdentity = Nothing};

-- | The current version of the origin access identity\'s information. For
-- example: E2QWRUHAPOMQZL.
gcfoairETag :: Lens' GetCloudFrontOriginAccessIdentity2014_11_06Response (Maybe Text)
gcfoairETag = lens _gcfoairETag (\ s a -> s{_gcfoairETag = a});

-- | The origin access identity\'s information.
gcfoairCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentity2014_11_06Response (Maybe CloudFrontOriginAccessIdentity)
gcfoairCloudFrontOriginAccessIdentity = lens _gcfoairCloudFrontOriginAccessIdentity (\ s a -> s{_gcfoairCloudFrontOriginAccessIdentity = a});
