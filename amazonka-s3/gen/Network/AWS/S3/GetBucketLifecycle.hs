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
-- Module      : Network.AWS.S3.GetBucketLifecycle
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the lifecycle configuration information set on the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLifecycle.html AWS API Reference> for GetBucketLifecycle.
module Network.AWS.S3.GetBucketLifecycle
    (
    -- * Creating a Request
      getBucketLifecycle
    , GetBucketLifecycle
    -- * Request Lenses
    , gBucket

    -- * Destructuring the Response
    , getBucketLifecycleResponse
    , GetBucketLifecycleResponse
    -- * Response Lenses
    , gblrsRules
    , gblrsResponseStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketLifecycle' smart constructor.
newtype GetBucketLifecycle = GetBucketLifecycle'
    { _gBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gBucket'
getBucketLifecycle
    :: BucketName -- ^ 'gBucket'
    -> GetBucketLifecycle
getBucketLifecycle pBucket_ =
    GetBucketLifecycle'
    { _gBucket = pBucket_
    }

-- | Undocumented member.
gBucket :: Lens' GetBucketLifecycle BucketName
gBucket = lens _gBucket (\ s a -> s{_gBucket = a});

instance AWSRequest GetBucketLifecycle where
        type Rs GetBucketLifecycle =
             GetBucketLifecycleResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketLifecycleResponse' <$>
                   (may (parseXMLList "Rule") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetBucketLifecycle where
        toHeaders = const mempty

instance ToPath GetBucketLifecycle where
        toPath GetBucketLifecycle'{..}
          = mconcat ["/", toBS _gBucket]

instance ToQuery GetBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'getBucketLifecycleResponse' smart constructor.
data GetBucketLifecycleResponse = GetBucketLifecycleResponse'
    { _gblrsRules          :: !(Maybe [Rule])
    , _gblrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketLifecycleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gblrsRules'
--
-- * 'gblrsResponseStatus'
getBucketLifecycleResponse
    :: Int -- ^ 'gblrsResponseStatus'
    -> GetBucketLifecycleResponse
getBucketLifecycleResponse pResponseStatus_ =
    GetBucketLifecycleResponse'
    { _gblrsRules = Nothing
    , _gblrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gblrsRules :: Lens' GetBucketLifecycleResponse [Rule]
gblrsRules = lens _gblrsRules (\ s a -> s{_gblrsRules = a}) . _Default . _Coerce;

-- | The response status code.
gblrsResponseStatus :: Lens' GetBucketLifecycleResponse Int
gblrsResponseStatus = lens _gblrsResponseStatus (\ s a -> s{_gblrsResponseStatus = a});
