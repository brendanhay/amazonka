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
-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle configuration from the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketLifecycle.html AWS API Reference> for DeleteBucketLifecycle.
module Network.AWS.S3.DeleteBucketLifecycle
    (
    -- * Creating a Request
      deleteBucketLifecycle
    , DeleteBucketLifecycle
    -- * Request Lenses
    , dblBucket

    -- * Destructuring the Response
    , deleteBucketLifecycleResponse
    , DeleteBucketLifecycleResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketLifecycle' smart constructor.
newtype DeleteBucketLifecycle = DeleteBucketLifecycle'
    { _dblBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBucketLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dblBucket'
deleteBucketLifecycle
    :: BucketName -- ^ 'dblBucket'
    -> DeleteBucketLifecycle
deleteBucketLifecycle pBucket_ =
    DeleteBucketLifecycle'
    { _dblBucket = pBucket_
    }

-- | Undocumented member.
dblBucket :: Lens' DeleteBucketLifecycle BucketName
dblBucket = lens _dblBucket (\ s a -> s{_dblBucket = a});

instance AWSRequest DeleteBucketLifecycle where
        type Rs DeleteBucketLifecycle =
             DeleteBucketLifecycleResponse
        request = delete s3
        response = receiveNull DeleteBucketLifecycleResponse'

instance ToHeaders DeleteBucketLifecycle where
        toHeaders = const mempty

instance ToPath DeleteBucketLifecycle where
        toPath DeleteBucketLifecycle'{..}
          = mconcat ["/", toBS _dblBucket]

instance ToQuery DeleteBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'deleteBucketLifecycleResponse' smart constructor.
data DeleteBucketLifecycleResponse =
    DeleteBucketLifecycleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBucketLifecycleResponse' with the minimum fields required to make a request.
--
deleteBucketLifecycleResponse
    :: DeleteBucketLifecycleResponse
deleteBucketLifecycleResponse = DeleteBucketLifecycleResponse'
