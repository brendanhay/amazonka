{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Delete an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
    (
    -- * Request
      DeleteCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , deleteCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , dcfoairqIfMatch
    , dcfoairqId

    -- * Response
    , DeleteCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , deleteCloudFrontOriginAccessIdentityResponse
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to delete a origin access identity.
--
-- /See:/ 'deleteCloudFrontOriginAccessIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcfoairqIfMatch'
--
-- * 'dcfoairqId'
data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity'
    { _dcfoairqIfMatch :: !(Maybe Text)
    , _dcfoairqId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCloudFrontOriginAccessIdentity' smart constructor.
deleteCloudFrontOriginAccessIdentity :: Text -> DeleteCloudFrontOriginAccessIdentity
deleteCloudFrontOriginAccessIdentity pId_ =
    DeleteCloudFrontOriginAccessIdentity'
    { _dcfoairqIfMatch = Nothing
    , _dcfoairqId = pId_
    }

-- | The value of the ETag header you received from a previous GET or PUT
-- request. For example: E2QWRUHAPOMQZL.
dcfoairqIfMatch :: Lens' DeleteCloudFrontOriginAccessIdentity (Maybe Text)
dcfoairqIfMatch = lens _dcfoairqIfMatch (\ s a -> s{_dcfoairqIfMatch = a});

-- | The origin access identity\'s id.
dcfoairqId :: Lens' DeleteCloudFrontOriginAccessIdentity Text
dcfoairqId = lens _dcfoairqId (\ s a -> s{_dcfoairqId = a});

instance AWSRequest
         DeleteCloudFrontOriginAccessIdentity where
        type Sv DeleteCloudFrontOriginAccessIdentity =
             CloudFront
        type Rs DeleteCloudFrontOriginAccessIdentity =
             DeleteCloudFrontOriginAccessIdentityResponse
        request = delete
        response
          = receiveNull
              DeleteCloudFrontOriginAccessIdentityResponse'

instance ToHeaders
         DeleteCloudFrontOriginAccessIdentity where
        toHeaders DeleteCloudFrontOriginAccessIdentity'{..}
          = mconcat ["If-Match" =# _dcfoairqIfMatch]

instance ToPath DeleteCloudFrontOriginAccessIdentity
         where
        toPath DeleteCloudFrontOriginAccessIdentity'{..}
          = mconcat
              ["/2015-04-17/origin-access-identity/cloudfront/",
               toText _dcfoairqId]

instance ToQuery DeleteCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | /See:/ 'deleteCloudFrontOriginAccessIdentityResponse' smart constructor.
data DeleteCloudFrontOriginAccessIdentityResponse =
    DeleteCloudFrontOriginAccessIdentityResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCloudFrontOriginAccessIdentityResponse' smart constructor.
deleteCloudFrontOriginAccessIdentityResponse :: DeleteCloudFrontOriginAccessIdentityResponse
deleteCloudFrontOriginAccessIdentityResponse =
    DeleteCloudFrontOriginAccessIdentityResponse'
