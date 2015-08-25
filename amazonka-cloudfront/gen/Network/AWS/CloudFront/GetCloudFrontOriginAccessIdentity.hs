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
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an origin access identity.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentity.html AWS API Reference> for GetCloudFrontOriginAccessIdentity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
    (
    -- * Creating a Request
      getCloudFrontOriginAccessIdentity
    , GetCloudFrontOriginAccessIdentity
    -- * Request Lenses
    , gcfoaiId

    -- * Destructuring the Response
    , getCloudFrontOriginAccessIdentityResponse
    , GetCloudFrontOriginAccessIdentityResponse
    -- * Response Lenses
    , gcfoairsETag
    , gcfoairsCloudFrontOriginAccessIdentity
    , gcfoairsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get an origin access identity\'s information.
--
-- /See:/ 'getCloudFrontOriginAccessIdentity' smart constructor.
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'
    { _gcfoaiId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfoaiId'
getCloudFrontOriginAccessIdentity
    :: Text -- ^ 'gcfoaiId'
    -> GetCloudFrontOriginAccessIdentity
getCloudFrontOriginAccessIdentity pId_ =
    GetCloudFrontOriginAccessIdentity'
    { _gcfoaiId = pId_
    }

-- | The identity\'s id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity Text
gcfoaiId = lens _gcfoaiId (\ s a -> s{_gcfoaiId = a});

instance AWSRequest GetCloudFrontOriginAccessIdentity
         where
        type Rs GetCloudFrontOriginAccessIdentity =
             GetCloudFrontOriginAccessIdentityResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetCloudFrontOriginAccessIdentityResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetCloudFrontOriginAccessIdentity
         where
        toHeaders = const mempty

instance ToPath GetCloudFrontOriginAccessIdentity
         where
        toPath GetCloudFrontOriginAccessIdentity'{..}
          = mconcat
              ["/2015-04-17/origin-access-identity/cloudfront/",
               toBS _gcfoaiId]

instance ToQuery GetCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getCloudFrontOriginAccessIdentityResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'
    { _gcfoairsETag                           :: !(Maybe Text)
    , _gcfoairsCloudFrontOriginAccessIdentity :: !(Maybe CloudFrontOriginAccessIdentity)
    , _gcfoairsStatus                         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfoairsETag'
--
-- * 'gcfoairsCloudFrontOriginAccessIdentity'
--
-- * 'gcfoairsStatus'
getCloudFrontOriginAccessIdentityResponse
    :: Int -- ^ 'gcfoairsStatus'
    -> GetCloudFrontOriginAccessIdentityResponse
getCloudFrontOriginAccessIdentityResponse pStatus_ =
    GetCloudFrontOriginAccessIdentityResponse'
    { _gcfoairsETag = Nothing
    , _gcfoairsCloudFrontOriginAccessIdentity = Nothing
    , _gcfoairsStatus = pStatus_
    }

-- | The current version of the origin access identity\'s information. For
-- example: E2QWRUHAPOMQZL.
gcfoairsETag :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe Text)
gcfoairsETag = lens _gcfoairsETag (\ s a -> s{_gcfoairsETag = a});

-- | The origin access identity\'s information.
gcfoairsCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
gcfoairsCloudFrontOriginAccessIdentity = lens _gcfoairsCloudFrontOriginAccessIdentity (\ s a -> s{_gcfoairsCloudFrontOriginAccessIdentity = a});

-- | The response status code.
gcfoairsStatus :: Lens' GetCloudFrontOriginAccessIdentityResponse Int
gcfoairsStatus = lens _gcfoairsStatus (\ s a -> s{_gcfoairsStatus = a});
