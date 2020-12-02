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
-- Module      : Network.AWS.S3.DeleteBucket
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the bucket. All objects (including all object versions and Delete Markers) in the bucket must be deleted before the bucket itself can be deleted.
module Network.AWS.S3.DeleteBucket
    (
    -- * Creating a Request
      deleteBucket
    , DeleteBucket
    -- * Request Lenses
    , dbBucket

    -- * Destructuring the Response
    , deleteBucketResponse
    , DeleteBucketResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucket' smart constructor.
newtype DeleteBucket = DeleteBucket'
  { _dbBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbBucket' - Undocumented member.
deleteBucket
    :: BucketName -- ^ 'dbBucket'
    -> DeleteBucket
deleteBucket pBucket_ = DeleteBucket' {_dbBucket = pBucket_}


-- | Undocumented member.
dbBucket :: Lens' DeleteBucket BucketName
dbBucket = lens _dbBucket (\ s a -> s{_dbBucket = a})

instance AWSRequest DeleteBucket where
        type Rs DeleteBucket = DeleteBucketResponse
        request = delete s3
        response = receiveNull DeleteBucketResponse'

instance Hashable DeleteBucket where

instance NFData DeleteBucket where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketResponse' with the minimum fields required to make a request.
--
deleteBucketResponse
    :: DeleteBucketResponse
deleteBucketResponse = DeleteBucketResponse'


instance NFData DeleteBucketResponse where
