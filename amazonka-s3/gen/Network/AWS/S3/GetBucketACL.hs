{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
      GetBucketACL
    , getBucketACL
    -- * Request Lenses
    , gbaBucket

    -- * Destructuring the Response
    , GetBucketACLResponse
    , getBucketACLResponse
    -- * Response Lenses
    , gbarsGrants
    , gbarsOwner
    , gbarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbaBucket'
newtype GetBucketACL = GetBucketACL'
    { _gbaBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketACL' smart constructor.
getBucketACL :: BucketName -> GetBucketACL
getBucketACL pBucket_ =
    GetBucketACL'
    { _gbaBucket = pBucket_
    }

-- | Undocumented member.
gbaBucket :: Lens' GetBucketACL BucketName
gbaBucket = lens _gbaBucket (\ s a -> s{_gbaBucket = a});

instance AWSRequest GetBucketACL where
        type Sv GetBucketACL = S3
        type Rs GetBucketACL = GetBucketACLResponse
        request = get
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbarsGrants'
--
-- * 'gbarsOwner'
--
-- * 'gbarsStatus'
data GetBucketACLResponse = GetBucketACLResponse'
    { _gbarsGrants :: !(Maybe [Grant])
    , _gbarsOwner  :: !(Maybe Owner)
    , _gbarsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketACLResponse' smart constructor.
getBucketACLResponse :: Int -> GetBucketACLResponse
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

-- | Undocumented member.
gbarsStatus :: Lens' GetBucketACLResponse Int
gbarsStatus = lens _gbarsStatus (\ s a -> s{_gbarsStatus = a});
