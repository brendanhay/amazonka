{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the bucket. All objects (including all object versions and
-- Delete Markers) in the bucket must be deleted before the bucket itself
-- can be deleted.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucket.html AWS API Reference> for DeleteBucket.
module Network.AWS.S3.DeleteBucket
    (
    -- * Creating a Request
      DeleteBucket
    , deleteBucket
    -- * Request Lenses
    , dbBucket

    -- * Destructuring the Response
    , DeleteBucketResponse
    , deleteBucketResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteBucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbBucket'
newtype DeleteBucket = DeleteBucket'
    { _dbBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucket' smart constructor.
deleteBucket :: BucketName -> DeleteBucket
deleteBucket pBucket_ =
    DeleteBucket'
    { _dbBucket = pBucket_
    }

-- | Undocumented member.
dbBucket :: Lens' DeleteBucket BucketName
dbBucket = lens _dbBucket (\ s a -> s{_dbBucket = a});

instance AWSRequest DeleteBucket where
        type Sv DeleteBucket = S3
        type Rs DeleteBucket = DeleteBucketResponse
        request = delete
        response = receiveNull DeleteBucketResponse'

instance ToHeaders DeleteBucket where
        toHeaders = const mempty

instance ToPath DeleteBucket where
        toPath DeleteBucket'{..}
          = mconcat ["/", toBS _dbBucket]

instance ToQuery DeleteBucket where
        toQuery = const mempty

-- | /See:/ 'deleteBucketResponse' smart constructor.
data DeleteBucketResponse =
    DeleteBucketResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketResponse' smart constructor.
deleteBucketResponse :: DeleteBucketResponse
deleteBucketResponse = DeleteBucketResponse'
