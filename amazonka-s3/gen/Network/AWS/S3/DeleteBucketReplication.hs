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
-- Module      : Network.AWS.S3.DeleteBucketReplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the replication configuration from the bucket.
module Network.AWS.S3.DeleteBucketReplication
    (
    -- * Creating a Request
      deleteBucketReplication
    , DeleteBucketReplication
    -- * Request Lenses
    , dbrBucket

    -- * Destructuring the Response
    , deleteBucketReplicationResponse
    , DeleteBucketReplicationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketReplication' smart constructor.
newtype DeleteBucketReplication = DeleteBucketReplication'
  { _dbrBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrBucket' - Undocumented member.
deleteBucketReplication
    :: BucketName -- ^ 'dbrBucket'
    -> DeleteBucketReplication
deleteBucketReplication pBucket_ =
  DeleteBucketReplication' {_dbrBucket = pBucket_}


-- | Undocumented member.
dbrBucket :: Lens' DeleteBucketReplication BucketName
dbrBucket = lens _dbrBucket (\ s a -> s{_dbrBucket = a})

instance AWSRequest DeleteBucketReplication where
        type Rs DeleteBucketReplication =
             DeleteBucketReplicationResponse
        request = delete s3
        response
          = receiveNull DeleteBucketReplicationResponse'

instance Hashable DeleteBucketReplication where

instance NFData DeleteBucketReplication where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketReplicationResponse' with the minimum fields required to make a request.
--
deleteBucketReplicationResponse
    :: DeleteBucketReplicationResponse
deleteBucketReplicationResponse = DeleteBucketReplicationResponse'


instance NFData DeleteBucketReplicationResponse where
