{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig2014_11_06
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

-- | Get the configuration information about an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentityConfig2014_11_06.html>
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig2014_11_06
    (
    -- * Request
      GetCloudFrontOriginAccessIdentityConfig2014_11_06
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentityConfig2014_11_06
    -- ** Request lenses
    , gcfoaicId

    -- * Response
    , GetCloudFrontOriginAccessIdentityConfig2014_11_06Response
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentityConfig2014_11_06Response
    -- ** Response lenses
    , gcfoaicrCloudFrontOriginAccessIdentityConfig
    , gcfoaicrETag
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'getCloudFrontOriginAccessIdentityConfig2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicId'
newtype GetCloudFrontOriginAccessIdentityConfig2014_11_06 = GetCloudFrontOriginAccessIdentityConfig2014_11_06'{_gcfoaicId :: Text} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentityConfig2014_11_06' smart constructor.
getCloudFrontOriginAccessIdentityConfig2014_11_06 :: Text -> GetCloudFrontOriginAccessIdentityConfig2014_11_06
getCloudFrontOriginAccessIdentityConfig2014_11_06 pId = GetCloudFrontOriginAccessIdentityConfig2014_11_06'{_gcfoaicId = pId};

-- | The identity\'s id.
gcfoaicId :: Lens' GetCloudFrontOriginAccessIdentityConfig2014_11_06 Text
gcfoaicId = lens _gcfoaicId (\ s a -> s{_gcfoaicId = a});

instance AWSRequest
         GetCloudFrontOriginAccessIdentityConfig2014_11_06
         where
        type Sv
               GetCloudFrontOriginAccessIdentityConfig2014_11_06
             = CloudFront
        type Rs
               GetCloudFrontOriginAccessIdentityConfig2014_11_06
             =
             GetCloudFrontOriginAccessIdentityConfig2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentityConfig2014_11_06Response'
                   <$>
                   x .@? "CloudFrontOriginAccessIdentityConfig" <*>
                     h .#? "ETag")

instance ToHeaders
         GetCloudFrontOriginAccessIdentityConfig2014_11_06
         where
        toHeaders = const mempty

instance ToPath
         GetCloudFrontOriginAccessIdentityConfig2014_11_06
         where
        toPath
          GetCloudFrontOriginAccessIdentityConfig2014_11_06'{..}
          = mconcat
              ["/2014-11-06/origin-access-identity/cloudfront/",
               toText _gcfoaicId, "/config"]

instance ToQuery
         GetCloudFrontOriginAccessIdentityConfig2014_11_06
         where
        toQuery = const mempty

-- | /See:/ 'getCloudFrontOriginAccessIdentityConfig2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicrCloudFrontOriginAccessIdentityConfig'
--
-- * 'gcfoaicrETag'
data GetCloudFrontOriginAccessIdentityConfig2014_11_06Response = GetCloudFrontOriginAccessIdentityConfig2014_11_06Response'{_gcfoaicrCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig, _gcfoaicrETag :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentityConfig2014_11_06Response' smart constructor.
getCloudFrontOriginAccessIdentityConfig2014_11_06Response :: GetCloudFrontOriginAccessIdentityConfig2014_11_06Response
getCloudFrontOriginAccessIdentityConfig2014_11_06Response = GetCloudFrontOriginAccessIdentityConfig2014_11_06Response'{_gcfoaicrCloudFrontOriginAccessIdentityConfig = Nothing, _gcfoaicrETag = Nothing};

-- | The origin access identity\'s configuration information.
gcfoaicrCloudFrontOriginAccessIdentityConfig :: Lens' GetCloudFrontOriginAccessIdentityConfig2014_11_06Response (Maybe CloudFrontOriginAccessIdentityConfig)
gcfoaicrCloudFrontOriginAccessIdentityConfig = lens _gcfoaicrCloudFrontOriginAccessIdentityConfig (\ s a -> s{_gcfoaicrCloudFrontOriginAccessIdentityConfig = a});

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gcfoaicrETag :: Lens' GetCloudFrontOriginAccessIdentityConfig2014_11_06Response (Maybe Text)
gcfoaicrETag = lens _gcfoaicrETag (\ s a -> s{_gcfoaicrETag = a});
