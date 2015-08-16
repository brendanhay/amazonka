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
-- Module      : Network.AWS.S3.DeleteBucketCORS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the cors configuration information set for the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketCORS.html AWS API Reference> for DeleteBucketCORS.
module Network.AWS.S3.DeleteBucketCORS
    (
    -- * Creating a Request
      deleteBucketCORS
    , DeleteBucketCORS
    -- * Request Lenses
    , dbcBucket

    -- * Destructuring the Response
    , deleteBucketCORSResponse
    , DeleteBucketCORSResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketCORS' smart constructor.
newtype DeleteBucketCORS = DeleteBucketCORS'
    { _dbcBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBucketCORS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbcBucket'
deleteBucketCORS
    :: BucketName -- ^ 'dbcBucket'
    -> DeleteBucketCORS
deleteBucketCORS pBucket_ =
    DeleteBucketCORS'
    { _dbcBucket = pBucket_
    }

-- | Undocumented member.
dbcBucket :: Lens' DeleteBucketCORS BucketName
dbcBucket = lens _dbcBucket (\ s a -> s{_dbcBucket = a});

instance AWSRequest DeleteBucketCORS where
        type Sv DeleteBucketCORS = S3
        type Rs DeleteBucketCORS = DeleteBucketCORSResponse
        request = delete
        response = receiveNull DeleteBucketCORSResponse'

instance ToHeaders DeleteBucketCORS where
        toHeaders = const mempty

instance ToPath DeleteBucketCORS where
        toPath DeleteBucketCORS'{..}
          = mconcat ["/", toBS _dbcBucket]

instance ToQuery DeleteBucketCORS where
        toQuery = const (mconcat ["cors"])

-- | /See:/ 'deleteBucketCORSResponse' smart constructor.
data DeleteBucketCORSResponse =
    DeleteBucketCORSResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBucketCORSResponse' with the minimum fields required to make a request.
--
deleteBucketCORSResponse
    :: DeleteBucketCORSResponse
deleteBucketCORSResponse = DeleteBucketCORSResponse'
