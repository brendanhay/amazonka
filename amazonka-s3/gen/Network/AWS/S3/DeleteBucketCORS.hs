{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.DeleteBucketCORS
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

-- | Deletes the cors configuration information set for the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketCORS.html>
module Network.AWS.S3.DeleteBucketCORS
    (
    -- * Request
      DeleteBucketCORS
    -- ** Request constructor
    , deleteBucketCORS
    -- ** Request lenses
    , dbcBucket

    -- * Response
    , DeleteBucketCORSResponse
    -- ** Response constructor
    , deleteBucketCORSResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteBucketCORS' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbcBucket'
newtype DeleteBucketCORS = DeleteBucketCORS'
    { _dbcBucket :: BucketName
    } deriving (Eq,Show)

-- | 'DeleteBucketCORS' smart constructor.
deleteBucketCORS :: BucketName -> DeleteBucketCORS
deleteBucketCORS pBucket =
    DeleteBucketCORS'
    { _dbcBucket = pBucket
    }

-- | FIXME: Undocumented member.
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
          = mconcat ["/", toText _dbcBucket]

instance ToQuery DeleteBucketCORS where
        toQuery = const (mconcat ["cors"])

-- | /See:/ 'deleteBucketCORSResponse' smart constructor.
data DeleteBucketCORSResponse =
    DeleteBucketCORSResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteBucketCORSResponse' smart constructor.
deleteBucketCORSResponse :: DeleteBucketCORSResponse
deleteBucketCORSResponse = DeleteBucketCORSResponse'
