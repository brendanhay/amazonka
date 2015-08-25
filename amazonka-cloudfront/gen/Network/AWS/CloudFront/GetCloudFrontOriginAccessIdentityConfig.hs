{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about an origin access identity.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentityConfig.html AWS API Reference> for GetCloudFrontOriginAccessIdentityConfig.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
    (
    -- * Creating a Request
      getCloudFrontOriginAccessIdentityConfig
    , GetCloudFrontOriginAccessIdentityConfig
    -- * Request Lenses
    , gcfoaicId

    -- * Destructuring the Response
    , getCloudFrontOriginAccessIdentityConfigResponse
    , GetCloudFrontOriginAccessIdentityConfigResponse
    -- * Response Lenses
    , gcfoaicrsCloudFrontOriginAccessIdentityConfig
    , gcfoaicrsETag
    , gcfoaicrsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get an origin access identity\'s configuration.
--
-- /See:/ 'getCloudFrontOriginAccessIdentityConfig' smart constructor.
newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'
    { _gcfoaicId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetCloudFrontOriginAccessIdentityConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfoaicId'
getCloudFrontOriginAccessIdentityConfig
    :: Text -- ^ 'gcfoaicId'
    -> GetCloudFrontOriginAccessIdentityConfig
getCloudFrontOriginAccessIdentityConfig pId_ =
    GetCloudFrontOriginAccessIdentityConfig'
    { _gcfoaicId = pId_
    }

-- | The identity\'s id.
gcfoaicId :: Lens' GetCloudFrontOriginAccessIdentityConfig Text
gcfoaicId = lens _gcfoaicId (\ s a -> s{_gcfoaicId = a});

instance AWSRequest
         GetCloudFrontOriginAccessIdentityConfig where
        type Rs GetCloudFrontOriginAccessIdentityConfig =
             GetCloudFrontOriginAccessIdentityConfigResponse
        request = get cloudFront
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
               toBS _gcfoaicId, "/config"]

instance ToQuery
         GetCloudFrontOriginAccessIdentityConfig where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'
    { _gcfoaicrsCloudFrontOriginAccessIdentityConfig :: !(Maybe CloudFrontOriginAccessIdentityConfig)
    , _gcfoaicrsETag                                 :: !(Maybe Text)
    , _gcfoaicrsStatus                               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetCloudFrontOriginAccessIdentityConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfoaicrsCloudFrontOriginAccessIdentityConfig'
--
-- * 'gcfoaicrsETag'
--
-- * 'gcfoaicrsStatus'
getCloudFrontOriginAccessIdentityConfigResponse
    :: Int -- ^ 'gcfoaicrsStatus'
    -> GetCloudFrontOriginAccessIdentityConfigResponse
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

-- | The response status code.
gcfoaicrsStatus :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse Int
gcfoaicrsStatus = lens _gcfoaicrsStatus (\ s a -> s{_gcfoaicrsStatus = a});
