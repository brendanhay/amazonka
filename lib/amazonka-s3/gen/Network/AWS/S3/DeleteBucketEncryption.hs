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
-- Module      : Network.AWS.S3.DeleteBucketEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the server-side encryption configuration from the bucket.
module Network.AWS.S3.DeleteBucketEncryption
    (
    -- * Creating a Request
      deleteBucketEncryption
    , DeleteBucketEncryption
    -- * Request Lenses
    , dbeBucket

    -- * Destructuring the Response
    , deleteBucketEncryptionResponse
    , DeleteBucketEncryptionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketEncryption' smart constructor.
newtype DeleteBucketEncryption = DeleteBucketEncryption'
  { _dbeBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbeBucket' - The name of the bucket containing the server-side encryption configuration to delete.
deleteBucketEncryption
    :: BucketName -- ^ 'dbeBucket'
    -> DeleteBucketEncryption
deleteBucketEncryption pBucket_ =
  DeleteBucketEncryption' {_dbeBucket = pBucket_}


-- | The name of the bucket containing the server-side encryption configuration to delete.
dbeBucket :: Lens' DeleteBucketEncryption BucketName
dbeBucket = lens _dbeBucket (\ s a -> s{_dbeBucket = a})

instance AWSRequest DeleteBucketEncryption where
        type Rs DeleteBucketEncryption =
             DeleteBucketEncryptionResponse
        request = delete s3
        response
          = receiveNull DeleteBucketEncryptionResponse'

instance Hashable DeleteBucketEncryption where

instance NFData DeleteBucketEncryption where

instance ToHeaders DeleteBucketEncryption where
        toHeaders = const mempty

instance ToPath DeleteBucketEncryption where
        toPath DeleteBucketEncryption'{..}
          = mconcat ["/", toBS _dbeBucket]

instance ToQuery DeleteBucketEncryption where
        toQuery = const (mconcat ["encryption"])

-- | /See:/ 'deleteBucketEncryptionResponse' smart constructor.
data DeleteBucketEncryptionResponse =
  DeleteBucketEncryptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketEncryptionResponse' with the minimum fields required to make a request.
--
deleteBucketEncryptionResponse
    :: DeleteBucketEncryptionResponse
deleteBucketEncryptionResponse = DeleteBucketEncryptionResponse'


instance NFData DeleteBucketEncryptionResponse where
