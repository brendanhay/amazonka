{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentityConfig.html>
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
    (
    -- * Request
      GetCloudFrontOriginAccessIdentityConfig
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentityConfig
    -- ** Request lenses
    , gcfoaicId

    -- * Response
    , GetCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response lenses
    , gcfoaicrCloudFrontOriginAccessIdentityConfig
    , gcfoaicrETag
    , gcfoaicrStatusCode
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to get an origin access identity\'s configuration.
--
-- /See:/ 'getCloudFrontOriginAccessIdentityConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicId'
newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'{_gcfoaicId :: Text} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentityConfig' smart constructor.
getCloudFrontOriginAccessIdentityConfig :: Text -> GetCloudFrontOriginAccessIdentityConfig
getCloudFrontOriginAccessIdentityConfig pId = GetCloudFrontOriginAccessIdentityConfig'{_gcfoaicId = pId};

-- | The identity\'s id.
gcfoaicId :: Lens' GetCloudFrontOriginAccessIdentityConfig Text
gcfoaicId = lens _gcfoaicId (\ s a -> s{_gcfoaicId = a});

instance AWSRequest
         GetCloudFrontOriginAccessIdentityConfig where
        type Sv GetCloudFrontOriginAccessIdentityConfig =
             CloudFront
        type Rs GetCloudFrontOriginAccessIdentityConfig =
             GetCloudFrontOriginAccessIdentityConfigResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentityConfigResponse' <$>
                   (x .@? "CloudFrontOriginAccessIdentityConfig") <*>
                     (h .#? "ETag")
                     <*> (pure (fromEnum s)))

instance ToHeaders
         GetCloudFrontOriginAccessIdentityConfig where
        toHeaders = const mempty

instance ToPath
         GetCloudFrontOriginAccessIdentityConfig where
        toPath GetCloudFrontOriginAccessIdentityConfig'{..}
          = mconcat
              ["/2014-11-06/origin-access-identity/cloudfront/",
               toText _gcfoaicId, "/config"]

instance ToQuery
         GetCloudFrontOriginAccessIdentityConfig where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicrCloudFrontOriginAccessIdentityConfig'
--
-- * 'gcfoaicrETag'
--
-- * 'gcfoaicrStatusCode'
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'{_gcfoaicrCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig, _gcfoaicrETag :: Maybe Text, _gcfoaicrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
getCloudFrontOriginAccessIdentityConfigResponse :: Int -> GetCloudFrontOriginAccessIdentityConfigResponse
getCloudFrontOriginAccessIdentityConfigResponse pStatusCode = GetCloudFrontOriginAccessIdentityConfigResponse'{_gcfoaicrCloudFrontOriginAccessIdentityConfig = Nothing, _gcfoaicrETag = Nothing, _gcfoaicrStatusCode = pStatusCode};

-- | The origin access identity\'s configuration information.
gcfoaicrCloudFrontOriginAccessIdentityConfig :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe CloudFrontOriginAccessIdentityConfig)
gcfoaicrCloudFrontOriginAccessIdentityConfig = lens _gcfoaicrCloudFrontOriginAccessIdentityConfig (\ s a -> s{_gcfoaicrCloudFrontOriginAccessIdentityConfig = a});

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gcfoaicrETag :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe Text)
gcfoaicrETag = lens _gcfoaicrETag (\ s a -> s{_gcfoaicrETag = a});

-- | FIXME: Undocumented member.
gcfoaicrStatusCode :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse Int
gcfoaicrStatusCode = lens _gcfoaicrStatusCode (\ s a -> s{_gcfoaicrStatusCode = a});
