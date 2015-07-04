{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.S3.DeleteBucketReplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | FIXME: Undocumented operation.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketReplication.html>
module Network.AWS.S3.DeleteBucketReplication
    (
    -- * Request
      DeleteBucketReplication
    -- ** Request constructor
    , deleteBucketReplication
    -- ** Request lenses
    , dbrBucket

    -- * Response
    , DeleteBucketReplicationResponse
    -- ** Response constructor
    , deleteBucketReplicationResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteBucketReplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbrBucket'
newtype DeleteBucketReplication = DeleteBucketReplication'
    { _dbrBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteBucketReplication' smart constructor.
deleteBucketReplication :: BucketName -> DeleteBucketReplication
deleteBucketReplication pBucket =
    DeleteBucketReplication'
    { _dbrBucket = pBucket
    }

-- | FIXME: Undocumented member.
dbrBucket :: Lens' DeleteBucketReplication BucketName
dbrBucket = lens _dbrBucket (\ s a -> s{_dbrBucket = a});

instance AWSRequest DeleteBucketReplication where
        type Sv DeleteBucketReplication = S3
        type Rs DeleteBucketReplication =
             DeleteBucketReplicationResponse
        request = delete
        response
          = receiveNull DeleteBucketReplicationResponse'

instance ToHeaders DeleteBucketReplication where
        toHeaders = const mempty

instance ToPath DeleteBucketReplication where
        toPath DeleteBucketReplication'{..}
          = mconcat ["/", toText _dbrBucket]

instance ToQuery DeleteBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'deleteBucketReplicationResponse' smart constructor.
data DeleteBucketReplicationResponse =
    DeleteBucketReplicationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketReplicationResponse' smart constructor.
deleteBucketReplicationResponse :: DeleteBucketReplicationResponse
deleteBucketReplicationResponse = DeleteBucketReplicationResponse'
