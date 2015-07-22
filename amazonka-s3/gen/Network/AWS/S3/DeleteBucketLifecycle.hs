{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle configuration from the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketLifecycle.html>
module Network.AWS.S3.DeleteBucketLifecycle
    (
    -- * Request
      DeleteBucketLifecycle
    -- ** Request constructor
    , deleteBucketLifecycle
    -- ** Request lenses
    , dblrqBucket

    -- * Response
    , DeleteBucketLifecycleResponse
    -- ** Response constructor
    , deleteBucketLifecycleResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteBucketLifecycle' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dblrqBucket'
newtype DeleteBucketLifecycle = DeleteBucketLifecycle'
    { _dblrqBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteBucketLifecycle' smart constructor.
deleteBucketLifecycle :: BucketName -> DeleteBucketLifecycle
deleteBucketLifecycle pBucket_ =
    DeleteBucketLifecycle'
    { _dblrqBucket = pBucket_
    }

-- | FIXME: Undocumented member.
dblrqBucket :: Lens' DeleteBucketLifecycle BucketName
dblrqBucket = lens _dblrqBucket (\ s a -> s{_dblrqBucket = a});

instance AWSRequest DeleteBucketLifecycle where
        type Sv DeleteBucketLifecycle = S3
        type Rs DeleteBucketLifecycle =
             DeleteBucketLifecycleResponse
        request = delete
        response = receiveNull DeleteBucketLifecycleResponse'

instance ToHeaders DeleteBucketLifecycle where
        toHeaders = const mempty

instance ToPath DeleteBucketLifecycle where
        toPath DeleteBucketLifecycle'{..}
          = mconcat ["/", toText _dblrqBucket]

instance ToQuery DeleteBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'deleteBucketLifecycleResponse' smart constructor.
data DeleteBucketLifecycleResponse =
    DeleteBucketLifecycleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketLifecycleResponse' smart constructor.
deleteBucketLifecycleResponse :: DeleteBucketLifecycleResponse
deleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
