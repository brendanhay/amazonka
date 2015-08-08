{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
      GetBucketLifecycle
    , getBucketLifecycle
    -- * Request Lenses
    , gBucket

    -- * Destructuring the Response
    , GetBucketLifecycleResponse
    , getBucketLifecycleResponse
    -- * Response Lenses
    , gblrsRules
    , gblrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketLifecycle' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gBucket'
newtype GetBucketLifecycle = GetBucketLifecycle'
    { _gBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketLifecycle' smart constructor.
getBucketLifecycle :: BucketName -> GetBucketLifecycle
getBucketLifecycle pBucket_ =
    GetBucketLifecycle'
    { _gBucket = pBucket_
    }

-- | Undocumented member.
gBucket :: Lens' GetBucketLifecycle BucketName
gBucket = lens _gBucket (\ s a -> s{_gBucket = a});

instance AWSRequest GetBucketLifecycle where
        type Sv GetBucketLifecycle = S3
        type Rs GetBucketLifecycle =
             GetBucketLifecycleResponse
        request = get
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrsRules'
--
-- * 'gblrsStatus'
data GetBucketLifecycleResponse = GetBucketLifecycleResponse'
    { _gblrsRules  :: !(Maybe [Rule])
    , _gblrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketLifecycleResponse' smart constructor.
getBucketLifecycleResponse :: Int -> GetBucketLifecycleResponse
getBucketLifecycleResponse pStatus_ =
    GetBucketLifecycleResponse'
    { _gblrsRules = Nothing
    , _gblrsStatus = pStatus_
    }

-- | Undocumented member.
gblrsRules :: Lens' GetBucketLifecycleResponse [Rule]
gblrsRules = lens _gblrsRules (\ s a -> s{_gblrsRules = a}) . _Default . _Coerce;

-- | Undocumented member.
gblrsStatus :: Lens' GetBucketLifecycleResponse Int
gblrsStatus = lens _gblrsStatus (\ s a -> s{_gblrsStatus = a});
