{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an origin access identity.
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
    , gcfoairsETag
    , gcfoairsCloudFrontOriginAccessIdentity
    , gcfoairsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get an origin access identity\'s information.
--
-- /See:/ 'getCloudFrontOriginAccessIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaiId'
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'
    { _gcfoaiId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCloudFrontOriginAccessIdentity' smart constructor.
getCloudFrontOriginAccessIdentity :: Text -> GetCloudFrontOriginAccessIdentity
getCloudFrontOriginAccessIdentity pId_ =
    GetCloudFrontOriginAccessIdentity'
    { _gcfoaiId = pId_
    }

-- | The identity\'s id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity Text
gcfoaiId = lens _gcfoaiId (\ s a -> s{_gcfoaiId = a});

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
               toText _gcfoaiId]

instance ToQuery GetCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getCloudFrontOriginAccessIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoairsETag'
--
-- * 'gcfoairsCloudFrontOriginAccessIdentity'
--
-- * 'gcfoairsStatus'
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'
    { _gcfoairsETag                           :: !(Maybe Text)
    , _gcfoairsCloudFrontOriginAccessIdentity :: !(Maybe CloudFrontOriginAccessIdentity)
    , _gcfoairsStatus                         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCloudFrontOriginAccessIdentityResponse' smart constructor.
getCloudFrontOriginAccessIdentityResponse :: Int -> GetCloudFrontOriginAccessIdentityResponse
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

-- | FIXME: Undocumented member.
gcfoairsStatus :: Lens' GetCloudFrontOriginAccessIdentityResponse Int
gcfoairsStatus = lens _gcfoairsStatus (\ s a -> s{_gcfoairsStatus = a});
