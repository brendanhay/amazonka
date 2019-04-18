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
-- Module      : Network.AWS.S3.GetBucketPolicyStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy status for an Amazon S3 bucket, indicating whether the bucket is public.
--
--
module Network.AWS.S3.GetBucketPolicyStatus
    (
    -- * Creating a Request
      getBucketPolicyStatus
    , GetBucketPolicyStatus
    -- * Request Lenses
    , gbpsBucket

    -- * Destructuring the Response
    , getBucketPolicyStatusResponse
    , GetBucketPolicyStatusResponse
    -- * Response Lenses
    , gbpsrsPolicyStatus
    , gbpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketPolicyStatus' smart constructor.
newtype GetBucketPolicyStatus = GetBucketPolicyStatus'
  { _gbpsBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketPolicyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpsBucket' - The name of the Amazon S3 bucket whose policy status you want to retrieve.
getBucketPolicyStatus
    :: BucketName -- ^ 'gbpsBucket'
    -> GetBucketPolicyStatus
getBucketPolicyStatus pBucket_ = GetBucketPolicyStatus' {_gbpsBucket = pBucket_}


-- | The name of the Amazon S3 bucket whose policy status you want to retrieve.
gbpsBucket :: Lens' GetBucketPolicyStatus BucketName
gbpsBucket = lens _gbpsBucket (\ s a -> s{_gbpsBucket = a})

instance AWSRequest GetBucketPolicyStatus where
        type Rs GetBucketPolicyStatus =
             GetBucketPolicyStatusResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketPolicyStatusResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetBucketPolicyStatus where

instance NFData GetBucketPolicyStatus where

instance ToHeaders GetBucketPolicyStatus where
        toHeaders = const mempty

instance ToPath GetBucketPolicyStatus where
        toPath GetBucketPolicyStatus'{..}
          = mconcat ["/", toBS _gbpsBucket]

instance ToQuery GetBucketPolicyStatus where
        toQuery = const (mconcat ["policyStatus"])

-- | /See:/ 'getBucketPolicyStatusResponse' smart constructor.
data GetBucketPolicyStatusResponse = GetBucketPolicyStatusResponse'
  { _gbpsrsPolicyStatus   :: !(Maybe PolicyStatus)
  , _gbpsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketPolicyStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpsrsPolicyStatus' - The policy status for the specified bucket.
--
-- * 'gbpsrsResponseStatus' - -- | The response status code.
getBucketPolicyStatusResponse
    :: Int -- ^ 'gbpsrsResponseStatus'
    -> GetBucketPolicyStatusResponse
getBucketPolicyStatusResponse pResponseStatus_ =
  GetBucketPolicyStatusResponse'
    {_gbpsrsPolicyStatus = Nothing, _gbpsrsResponseStatus = pResponseStatus_}


-- | The policy status for the specified bucket.
gbpsrsPolicyStatus :: Lens' GetBucketPolicyStatusResponse (Maybe PolicyStatus)
gbpsrsPolicyStatus = lens _gbpsrsPolicyStatus (\ s a -> s{_gbpsrsPolicyStatus = a})

-- | -- | The response status code.
gbpsrsResponseStatus :: Lens' GetBucketPolicyStatusResponse Int
gbpsrsResponseStatus = lens _gbpsrsResponseStatus (\ s a -> s{_gbpsrsResponseStatus = a})

instance NFData GetBucketPolicyStatusResponse where
