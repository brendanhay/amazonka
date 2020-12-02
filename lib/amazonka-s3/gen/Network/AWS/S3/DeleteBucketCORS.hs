{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketCORS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @cors@ configuration information set for the bucket.
--
--
-- To use this operation, you must have permission to perform the @s3:PutBucketCORS@ action. The bucket owner has this permission by default and can grant this permission to others.
--
-- For information about @cors@ , see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- __Related Resources:__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html RESTOPTIONSobject>
module Network.AWS.S3.DeleteBucketCORS
  ( -- * Creating a Request
    deleteBucketCORS,
    DeleteBucketCORS,

    -- * Request Lenses
    dbcExpectedBucketOwner,
    dbcBucket,

    -- * Destructuring the Response
    deleteBucketCORSResponse,
    DeleteBucketCORSResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketCORS' smart constructor.
data DeleteBucketCORS = DeleteBucketCORS'
  { _dbcExpectedBucketOwner ::
      !(Maybe Text),
    _dbcBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketCORS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbcExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbcBucket' - Specifies the bucket whose @cors@ configuration is being deleted.
deleteBucketCORS ::
  -- | 'dbcBucket'
  BucketName ->
  DeleteBucketCORS
deleteBucketCORS pBucket_ =
  DeleteBucketCORS'
    { _dbcExpectedBucketOwner = Nothing,
      _dbcBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbcExpectedBucketOwner :: Lens' DeleteBucketCORS (Maybe Text)
dbcExpectedBucketOwner = lens _dbcExpectedBucketOwner (\s a -> s {_dbcExpectedBucketOwner = a})

-- | Specifies the bucket whose @cors@ configuration is being deleted.
dbcBucket :: Lens' DeleteBucketCORS BucketName
dbcBucket = lens _dbcBucket (\s a -> s {_dbcBucket = a})

instance AWSRequest DeleteBucketCORS where
  type Rs DeleteBucketCORS = DeleteBucketCORSResponse
  request = delete s3
  response = receiveNull DeleteBucketCORSResponse'

instance Hashable DeleteBucketCORS

instance NFData DeleteBucketCORS

instance ToHeaders DeleteBucketCORS where
  toHeaders DeleteBucketCORS' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbcExpectedBucketOwner]

instance ToPath DeleteBucketCORS where
  toPath DeleteBucketCORS' {..} = mconcat ["/", toBS _dbcBucket]

instance ToQuery DeleteBucketCORS where
  toQuery = const (mconcat ["cors"])

-- | /See:/ 'deleteBucketCORSResponse' smart constructor.
data DeleteBucketCORSResponse = DeleteBucketCORSResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketCORSResponse' with the minimum fields required to make a request.
deleteBucketCORSResponse ::
  DeleteBucketCORSResponse
deleteBucketCORSResponse = DeleteBucketCORSResponse'

instance NFData DeleteBucketCORSResponse
