{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about an origin access identity.
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
    , gcfoaicrsCloudFrontOriginAccessIdentityConfig
    , gcfoaicrsETag
    , gcfoaicrsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get an origin access identity\'s configuration.
--
-- /See:/ 'getCloudFrontOriginAccessIdentityConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicId'
newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'
    { _gcfoaicId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCloudFrontOriginAccessIdentityConfig' smart constructor.
getCloudFrontOriginAccessIdentityConfig :: Text -> GetCloudFrontOriginAccessIdentityConfig
getCloudFrontOriginAccessIdentityConfig pId_ =
    GetCloudFrontOriginAccessIdentityConfig'
    { _gcfoaicId = pId_
    }

-- | The identity\'s id.
gcfoaicId :: Lens' GetCloudFrontOriginAccessIdentityConfig Text
gcfoaicId = lens _gcfoaicId (\ s a -> s{_gcfoaicId = a});

instance AWSRequest
         GetCloudFrontOriginAccessIdentityConfig where
        type Sv GetCloudFrontOriginAccessIdentityConfig =
             CloudFront
        type Rs GetCloudFrontOriginAccessIdentityConfig =
             GetCloudFrontOriginAccessIdentityConfigResponse
        request
          = get "GetCloudFrontOriginAccessIdentityConfig"
        response
          = receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentityConfigResponse' <$>
                   (parseXML x) <*> (h .#? "ETag") <*>
                     (pure (fromEnum s)))

instance ToHeaders
         GetCloudFrontOriginAccessIdentityConfig where
        toHeaders = const mempty

instance ToPath
         GetCloudFrontOriginAccessIdentityConfig where
        toPath GetCloudFrontOriginAccessIdentityConfig'{..}
          = mconcat
              ["/2015-04-17/origin-access-identity/cloudfront/",
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
-- * 'gcfoaicrsCloudFrontOriginAccessIdentityConfig'
--
-- * 'gcfoaicrsETag'
--
-- * 'gcfoaicrsStatus'
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'
    { _gcfoaicrsCloudFrontOriginAccessIdentityConfig :: !(Maybe CloudFrontOriginAccessIdentityConfig)
    , _gcfoaicrsETag                                 :: !(Maybe Text)
    , _gcfoaicrsStatus                               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
getCloudFrontOriginAccessIdentityConfigResponse :: Int -> GetCloudFrontOriginAccessIdentityConfigResponse
getCloudFrontOriginAccessIdentityConfigResponse pStatus_ =
    GetCloudFrontOriginAccessIdentityConfigResponse'
    { _gcfoaicrsCloudFrontOriginAccessIdentityConfig = Nothing
    , _gcfoaicrsETag = Nothing
    , _gcfoaicrsStatus = pStatus_
    }

-- | The origin access identity\'s configuration information.
gcfoaicrsCloudFrontOriginAccessIdentityConfig :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe CloudFrontOriginAccessIdentityConfig)
gcfoaicrsCloudFrontOriginAccessIdentityConfig = lens _gcfoaicrsCloudFrontOriginAccessIdentityConfig (\ s a -> s{_gcfoaicrsCloudFrontOriginAccessIdentityConfig = a});

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gcfoaicrsETag :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe Text)
gcfoaicrsETag = lens _gcfoaicrsETag (\ s a -> s{_gcfoaicrsETag = a});

-- | FIXME: Undocumented member.
gcfoaicrsStatus :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse Int
gcfoaicrsStatus = lens _gcfoaicrsStatus (\ s a -> s{_gcfoaicrsStatus = a});
