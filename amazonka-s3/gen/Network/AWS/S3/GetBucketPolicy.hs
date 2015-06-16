{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the policy of a specified bucket.
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
    , gbprPolicy
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.S3.Types

-- | /See:/ 'getBucketPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpBucket'
newtype GetBucketPolicy = GetBucketPolicy'{_gbpBucket :: BucketName} deriving (Eq, Read, Show)

-- | 'GetBucketPolicy' smart constructor.
getBucketPolicy :: BucketName -> GetBucketPolicy
getBucketPolicy pBucket = GetBucketPolicy'{_gbpBucket = pBucket};

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
                 GetBucketPolicyResponse' <$> (x .@? "Policy"))

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
-- * 'gbprPolicy'
newtype GetBucketPolicyResponse = GetBucketPolicyResponse'{_gbprPolicy :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetBucketPolicyResponse' smart constructor.
getBucketPolicyResponse :: GetBucketPolicyResponse
getBucketPolicyResponse = GetBucketPolicyResponse'{_gbprPolicy = Nothing};

-- | The bucket policy as a JSON document.
gbprPolicy :: Lens' GetBucketPolicyResponse (Maybe Text)
gbprPolicy = lens _gbprPolicy (\ s a -> s{_gbprPolicy = a});
