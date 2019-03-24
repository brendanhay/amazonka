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
-- Module      : Network.AWS.Shield.AssociateDRTLogBucket
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response team (DRT) to access the specified Amazon S3 bucket containing your flow logs. You can associate up to 10 Amazon S3 buckets with your subscription.
--
--
-- To use the services of the DRT and make an @AssociateDRTLogBucket@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> .
--
module Network.AWS.Shield.AssociateDRTLogBucket
    (
    -- * Creating a Request
      associateDRTLogBucket
    , AssociateDRTLogBucket
    -- * Request Lenses
    , adrtlbLogBucket

    -- * Destructuring the Response
    , associateDRTLogBucketResponse
    , AssociateDRTLogBucketResponse
    -- * Response Lenses
    , adrtlbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'associateDRTLogBucket' smart constructor.
newtype AssociateDRTLogBucket = AssociateDRTLogBucket'
  { _adrtlbLogBucket :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDRTLogBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrtlbLogBucket' - The Amazon S3 bucket that contains your flow logs.
associateDRTLogBucket
    :: Text -- ^ 'adrtlbLogBucket'
    -> AssociateDRTLogBucket
associateDRTLogBucket pLogBucket_ =
  AssociateDRTLogBucket' {_adrtlbLogBucket = pLogBucket_}


-- | The Amazon S3 bucket that contains your flow logs.
adrtlbLogBucket :: Lens' AssociateDRTLogBucket Text
adrtlbLogBucket = lens _adrtlbLogBucket (\ s a -> s{_adrtlbLogBucket = a})

instance AWSRequest AssociateDRTLogBucket where
        type Rs AssociateDRTLogBucket =
             AssociateDRTLogBucketResponse
        request = postJSON shield
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateDRTLogBucketResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateDRTLogBucket where

instance NFData AssociateDRTLogBucket where

instance ToHeaders AssociateDRTLogBucket where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.AssociateDRTLogBucket" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateDRTLogBucket where
        toJSON AssociateDRTLogBucket'{..}
          = object
              (catMaybes [Just ("LogBucket" .= _adrtlbLogBucket)])

instance ToPath AssociateDRTLogBucket where
        toPath = const "/"

instance ToQuery AssociateDRTLogBucket where
        toQuery = const mempty

-- | /See:/ 'associateDRTLogBucketResponse' smart constructor.
newtype AssociateDRTLogBucketResponse = AssociateDRTLogBucketResponse'
  { _adrtlbrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDRTLogBucketResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrtlbrsResponseStatus' - -- | The response status code.
associateDRTLogBucketResponse
    :: Int -- ^ 'adrtlbrsResponseStatus'
    -> AssociateDRTLogBucketResponse
associateDRTLogBucketResponse pResponseStatus_ =
  AssociateDRTLogBucketResponse' {_adrtlbrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
adrtlbrsResponseStatus :: Lens' AssociateDRTLogBucketResponse Int
adrtlbrsResponseStatus = lens _adrtlbrsResponseStatus (\ s a -> s{_adrtlbrsResponseStatus = a})

instance NFData AssociateDRTLogBucketResponse where
