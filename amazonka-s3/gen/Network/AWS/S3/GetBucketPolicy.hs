{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the policy of a specified bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketPolicy.html>
module Network.AWS.S3.GetBucketPolicy
    (
    -- * Request
      GetBucketPolicy
    -- ** Request constructor
    , getBucketPolicy
    -- ** Request lenses
    , gbpBucket

    -- * Response
    , GetBucketPolicyResponse
    -- ** Response constructor
    , getBucketPolicyResponse
    -- ** Response lenses
    , gbprsPolicy
    , gbprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpBucket'
newtype GetBucketPolicy = GetBucketPolicy'
    { _gbpBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketPolicy' smart constructor.
getBucketPolicy :: BucketName -> GetBucketPolicy
getBucketPolicy pBucket_ =
    GetBucketPolicy'
    { _gbpBucket = pBucket_
    }

-- | FIXME: Undocumented member.
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
          = mconcat ["/", toText _gbpBucket]

instance ToQuery GetBucketPolicy where
        toQuery = const (mconcat ["policy"])

-- | /See:/ 'getBucketPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbprsPolicy'
--
-- * 'gbprsStatus'
data GetBucketPolicyResponse = GetBucketPolicyResponse'
    { _gbprsPolicy :: !(Maybe Text)
    , _gbprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketPolicyResponse' smart constructor.
getBucketPolicyResponse :: Int -> GetBucketPolicyResponse
getBucketPolicyResponse pStatus_ =
    GetBucketPolicyResponse'
    { _gbprsPolicy = Nothing
    , _gbprsStatus = pStatus_
    }

-- | The bucket policy as a JSON document.
gbprsPolicy :: Lens' GetBucketPolicyResponse (Maybe Text)
gbprsPolicy = lens _gbprsPolicy (\ s a -> s{_gbprsPolicy = a});

-- | FIXME: Undocumented member.
gbprsStatus :: Lens' GetBucketPolicyResponse Int
gbprsStatus = lens _gbprsStatus (\ s a -> s{_gbprsStatus = a});
