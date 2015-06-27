{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.DeleteBucketPolicy
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

-- | Deletes the policy from the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketPolicy.html>
module Network.AWS.S3.DeleteBucketPolicy
    (
    -- * Request
      DeleteBucketPolicy
    -- ** Request constructor
    , deleteBucketPolicy
    -- ** Request lenses
    , dbpBucket

    -- * Response
    , DeleteBucketPolicyResponse
    -- ** Response constructor
    , deleteBucketPolicyResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteBucketPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpBucket'
newtype DeleteBucketPolicy = DeleteBucketPolicy'
    { _dbpBucket :: BucketName
    } deriving (Eq,Read,Show)

-- | 'DeleteBucketPolicy' smart constructor.
deleteBucketPolicy :: BucketName -> DeleteBucketPolicy
deleteBucketPolicy pBucket =
    DeleteBucketPolicy'
    { _dbpBucket = pBucket
    }

-- | FIXME: Undocumented member.
dbpBucket :: Lens' DeleteBucketPolicy BucketName
dbpBucket = lens _dbpBucket (\ s a -> s{_dbpBucket = a});

instance AWSRequest DeleteBucketPolicy where
        type Sv DeleteBucketPolicy = S3
        type Rs DeleteBucketPolicy =
             DeleteBucketPolicyResponse
        request = delete
        response = receiveNull DeleteBucketPolicyResponse'

instance ToHeaders DeleteBucketPolicy where
        toHeaders = const mempty

instance ToPath DeleteBucketPolicy where
        toPath DeleteBucketPolicy'{..}
          = mconcat ["/", toText _dbpBucket]

instance ToQuery DeleteBucketPolicy where
        toQuery = const (mconcat ["policy"])

-- | /See:/ 'deleteBucketPolicyResponse' smart constructor.
data DeleteBucketPolicyResponse =
    DeleteBucketPolicyResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteBucketPolicyResponse' smart constructor.
deleteBucketPolicyResponse :: DeleteBucketPolicyResponse
deleteBucketPolicyResponse = DeleteBucketPolicyResponse'
