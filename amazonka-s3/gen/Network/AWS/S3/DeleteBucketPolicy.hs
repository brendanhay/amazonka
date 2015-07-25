{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the policy from the bucket.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketPolicy' smart constructor.
deleteBucketPolicy :: BucketName -> DeleteBucketPolicy
deleteBucketPolicy pBucket_ =
    DeleteBucketPolicy'
    { _dbpBucket = pBucket_
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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketPolicyResponse' smart constructor.
deleteBucketPolicyResponse :: DeleteBucketPolicyResponse
deleteBucketPolicyResponse = DeleteBucketPolicyResponse'
