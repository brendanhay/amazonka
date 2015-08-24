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
-- Module      : Network.AWS.S3.GetBucketACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the access control policy for the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketACL.html AWS API Reference> for GetBucketACL.
module Network.AWS.S3.GetBucketACL
    (
    -- * Creating a Request
      getBucketACL
    , GetBucketACL
    -- * Request Lenses
    , gbaBucket

    -- * Destructuring the Response
    , getBucketACLResponse
    , GetBucketACLResponse
    -- * Response Lenses
    , gbarsGrants
    , gbarsOwner
    , gbarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketACL' smart constructor.
newtype GetBucketACL = GetBucketACL'
    { _gbaBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbaBucket'
getBucketACL
    :: BucketName -- ^ 'gbaBucket'
    -> GetBucketACL
getBucketACL pBucket_ =
    GetBucketACL'
    { _gbaBucket = pBucket_
    }

-- | Undocumented member.
gbaBucket :: Lens' GetBucketACL BucketName
gbaBucket = lens _gbaBucket (\ s a -> s{_gbaBucket = a});

instance AWSRequest GetBucketACL where
        type Rs GetBucketACL = GetBucketACLResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketACLResponse' <$>
                   (x .@? "AccessControlList" .!@ mempty >>=
                      may (parseXMLList "Grant"))
                     <*> (x .@? "Owner")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetBucketACL where
        toHeaders = const mempty

instance ToPath GetBucketACL where
        toPath GetBucketACL'{..}
          = mconcat ["/", toBS _gbaBucket]

instance ToQuery GetBucketACL where
        toQuery = const (mconcat ["acl"])

-- | /See:/ 'getBucketACLResponse' smart constructor.
data GetBucketACLResponse = GetBucketACLResponse'
    { _gbarsGrants :: !(Maybe [Grant])
    , _gbarsOwner  :: !(Maybe Owner)
    , _gbarsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbarsGrants'
--
-- * 'gbarsOwner'
--
-- * 'gbarsStatus'
getBucketACLResponse
    :: Int -- ^ 'gbarsStatus'
    -> GetBucketACLResponse
getBucketACLResponse pStatus_ =
    GetBucketACLResponse'
    { _gbarsGrants = Nothing
    , _gbarsOwner = Nothing
    , _gbarsStatus = pStatus_
    }

-- | A list of grants.
gbarsGrants :: Lens' GetBucketACLResponse [Grant]
gbarsGrants = lens _gbarsGrants (\ s a -> s{_gbarsGrants = a}) . _Default . _Coerce;

-- | Undocumented member.
gbarsOwner :: Lens' GetBucketACLResponse (Maybe Owner)
gbarsOwner = lens _gbarsOwner (\ s a -> s{_gbarsOwner = a});

-- | The response status code.
gbarsStatus :: Lens' GetBucketACLResponse Int
gbarsStatus = lens _gbarsStatus (\ s a -> s{_gbarsStatus = a});
