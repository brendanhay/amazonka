{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketReplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketReplication.html AWS API Reference> for DeleteBucketReplication.
module Network.AWS.S3.DeleteBucketReplication
    (
    -- * Creating a Request
      DeleteBucketReplication
    , deleteBucketReplication
    -- * Request Lenses
    , dbrBucket

    -- * Destructuring the Response
    , DeleteBucketReplicationResponse
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketReplication' smart constructor.
deleteBucketReplication :: BucketName -> DeleteBucketReplication
deleteBucketReplication pBucket_ =
    DeleteBucketReplication'
    { _dbrBucket = pBucket_
    }

-- | Undocumented member.
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
          = mconcat ["/", toBS _dbrBucket]

instance ToQuery DeleteBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'deleteBucketReplicationResponse' smart constructor.
data DeleteBucketReplicationResponse =
    DeleteBucketReplicationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketReplicationResponse' smart constructor.
deleteBucketReplicationResponse :: DeleteBucketReplicationResponse
deleteBucketReplicationResponse = DeleteBucketReplicationResponse'
