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
-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the policy of a specified bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketPolicy.html AWS API Reference> for GetBucketPolicy.
module Network.AWS.S3.GetBucketPolicy
    (
    -- * Creating a Request
      getBucketPolicy
    , GetBucketPolicy
    -- * Request Lenses
    , gbpBucket

    -- * Destructuring the Response
    , getBucketPolicyResponse
    , GetBucketPolicyResponse
    -- * Response Lenses
    , gbprsPolicy
    , gbprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketPolicy' smart constructor.
newtype GetBucketPolicy = GetBucketPolicy'
    { _gbpBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpBucket'
getBucketPolicy
    :: BucketName -- ^ 'gbpBucket'
    -> GetBucketPolicy
getBucketPolicy pBucket_ =
    GetBucketPolicy'
    { _gbpBucket = pBucket_
    }

-- | Undocumented member.
gbpBucket :: Lens' GetBucketPolicy BucketName
gbpBucket = lens _gbpBucket (\ s a -> s{_gbpBucket = a});

instance AWSRequest GetBucketPolicy where
        type Sv GetBucketPolicy = S3
        type Rs GetBucketPolicy = GetBucketPolicyResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketPolicyResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance ToHeaders GetBucketPolicy where
        toHeaders = const mempty

instance ToPath GetBucketPolicy where
        toPath GetBucketPolicy'{..}
          = mconcat ["/", toBS _gbpBucket]

instance ToQuery GetBucketPolicy where
        toQuery = const (mconcat ["policy"])

-- | /See:/ 'getBucketPolicyResponse' smart constructor.
data GetBucketPolicyResponse = GetBucketPolicyResponse'
    { _gbprsPolicy :: !(Maybe Text)
    , _gbprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbprsPolicy'
--
-- * 'gbprsStatus'
getBucketPolicyResponse
    :: Int -- ^ 'gbprsStatus'
    -> GetBucketPolicyResponse
getBucketPolicyResponse pStatus_ =
    GetBucketPolicyResponse'
    { _gbprsPolicy = Nothing
    , _gbprsStatus = pStatus_
    }

-- | The bucket policy as a JSON document.
gbprsPolicy :: Lens' GetBucketPolicyResponse (Maybe Text)
gbprsPolicy = lens _gbprsPolicy (\ s a -> s{_gbprsPolicy = a});

-- | The response status code.
gbprsStatus :: Lens' GetBucketPolicyResponse Int
gbprsStatus = lens _gbprsStatus (\ s a -> s{_gbprsStatus = a});
