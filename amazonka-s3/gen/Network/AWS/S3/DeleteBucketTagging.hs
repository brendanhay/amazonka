{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the tags from the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketTagging.html AWS API Reference> for DeleteBucketTagging.
module Network.AWS.S3.DeleteBucketTagging
    (
    -- * Creating a Request
      DeleteBucketTagging
    , deleteBucketTagging
    -- * Request Lenses
    , dbtBucket

    -- * Destructuring the Response
    , DeleteBucketTaggingResponse
    , deleteBucketTaggingResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteBucketTagging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtBucket'
newtype DeleteBucketTagging = DeleteBucketTagging'
    { _dbtBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketTagging' smart constructor.
deleteBucketTagging :: BucketName -> DeleteBucketTagging
deleteBucketTagging pBucket_ =
    DeleteBucketTagging'
    { _dbtBucket = pBucket_
    }

-- | Undocumented member.
dbtBucket :: Lens' DeleteBucketTagging BucketName
dbtBucket = lens _dbtBucket (\ s a -> s{_dbtBucket = a});

instance AWSRequest DeleteBucketTagging where
        type Sv DeleteBucketTagging = S3
        type Rs DeleteBucketTagging =
             DeleteBucketTaggingResponse
        request = delete
        response = receiveNull DeleteBucketTaggingResponse'

instance ToHeaders DeleteBucketTagging where
        toHeaders = const mempty

instance ToPath DeleteBucketTagging where
        toPath DeleteBucketTagging'{..}
          = mconcat ["/", toBS _dbtBucket]

instance ToQuery DeleteBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'deleteBucketTaggingResponse' smart constructor.
data DeleteBucketTaggingResponse =
    DeleteBucketTaggingResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketTaggingResponse' smart constructor.
deleteBucketTaggingResponse :: DeleteBucketTaggingResponse
deleteBucketTaggingResponse = DeleteBucketTaggingResponse'
