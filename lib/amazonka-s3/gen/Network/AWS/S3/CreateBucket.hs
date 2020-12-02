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
-- Module      : Network.AWS.S3.CreateBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new S3 bucket. To create a bucket, you must register with Amazon S3 and have a valid AWS Access Key ID to authenticate requests. Anonymous requests are never allowed to create buckets. By creating the bucket, you become the bucket owner.
--
--
-- Not every string is an acceptable bucket name. For information about bucket naming restrictions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html Working with Amazon S3 buckets> .
--
-- If you want to create an Amazon S3 on Outposts bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_control_CreateBucket.html Create Bucket> .
--
-- By default, the bucket is created in the US East (N. Virginia) Region. You can optionally specify a Region in the request body. You might choose a Region to optimize latency, minimize costs, or address regulatory requirements. For example, if you reside in Europe, you will probably find it advantageous to create buckets in the Europe (Ireland) Region. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html#access-bucket-intro Accessing a bucket> .
--
-- When creating a bucket using this operation, you can optionally specify the accounts or groups that should be granted specific permissions on the bucket. There are two ways to grant the appropriate permissions using the request headers.
--
--     * Specify a canned ACL using the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--     * Specify access permissions explicitly using the @x-amz-grant-read@ , @x-amz-grant-write@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. These headers map to the set of permissions Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access control list (ACL) overview> .
--
-- You specify each grantee as a type=value pair, where the type is one of the following:
--
--     * @id@ – if the value specified is the canonical user ID of an AWS account
--
--     * @uri@ – if you are granting permissions to a predefined group
--
--     * @emailAddress@ – if the value specified is the email address of an AWS account
--
--
--
-- For example, the following @x-amz-grant-read@ header grants the AWS accounts identified by account IDs permissions to read object data and its metadata:
--
-- @x-amz-grant-read: id="11112222333", id="444455556666" @
--
--
--
-- The following operations are related to @CreateBucket@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Network.AWS.S3.CreateBucket
  ( -- * Creating a Request
    createBucket,
    CreateBucket,

    -- * Request Lenses
    cbGrantReadACP,
    cbObjectLockEnabledForBucket,
    cbGrantWriteACP,
    cbGrantRead,
    cbGrantFullControl,
    cbCreateBucketConfiguration,
    cbGrantWrite,
    cbACL,
    cbBucket,

    -- * Destructuring the Response
    createBucketResponse,
    CreateBucketResponse,

    -- * Response Lenses
    cbrsLocation,
    cbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'createBucket' smart constructor.
data CreateBucket = CreateBucket'
  { _cbGrantReadACP :: !(Maybe Text),
    _cbObjectLockEnabledForBucket :: !(Maybe Bool),
    _cbGrantWriteACP :: !(Maybe Text),
    _cbGrantRead :: !(Maybe Text),
    _cbGrantFullControl :: !(Maybe Text),
    _cbCreateBucketConfiguration ::
      !(Maybe CreateBucketConfiguration),
    _cbGrantWrite :: !(Maybe Text),
    _cbACL :: !(Maybe BucketCannedACL),
    _cbBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbGrantReadACP' - Allows grantee to read the bucket ACL.
--
-- * 'cbObjectLockEnabledForBucket' - Specifies whether you want S3 Object Lock to be enabled for the new bucket.
--
-- * 'cbGrantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- * 'cbGrantRead' - Allows grantee to list the objects in the bucket.
--
-- * 'cbGrantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- * 'cbCreateBucketConfiguration' - The configuration information for the bucket.
--
-- * 'cbGrantWrite' - Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- * 'cbACL' - The canned ACL to apply to the bucket.
--
-- * 'cbBucket' - The name of the bucket to create.
createBucket ::
  -- | 'cbBucket'
  BucketName ->
  CreateBucket
createBucket pBucket_ =
  CreateBucket'
    { _cbGrantReadACP = Nothing,
      _cbObjectLockEnabledForBucket = Nothing,
      _cbGrantWriteACP = Nothing,
      _cbGrantRead = Nothing,
      _cbGrantFullControl = Nothing,
      _cbCreateBucketConfiguration = Nothing,
      _cbGrantWrite = Nothing,
      _cbACL = Nothing,
      _cbBucket = pBucket_
    }

-- | Allows grantee to read the bucket ACL.
cbGrantReadACP :: Lens' CreateBucket (Maybe Text)
cbGrantReadACP = lens _cbGrantReadACP (\s a -> s {_cbGrantReadACP = a})

-- | Specifies whether you want S3 Object Lock to be enabled for the new bucket.
cbObjectLockEnabledForBucket :: Lens' CreateBucket (Maybe Bool)
cbObjectLockEnabledForBucket = lens _cbObjectLockEnabledForBucket (\s a -> s {_cbObjectLockEnabledForBucket = a})

-- | Allows grantee to write the ACL for the applicable bucket.
cbGrantWriteACP :: Lens' CreateBucket (Maybe Text)
cbGrantWriteACP = lens _cbGrantWriteACP (\s a -> s {_cbGrantWriteACP = a})

-- | Allows grantee to list the objects in the bucket.
cbGrantRead :: Lens' CreateBucket (Maybe Text)
cbGrantRead = lens _cbGrantRead (\s a -> s {_cbGrantRead = a})

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
cbGrantFullControl :: Lens' CreateBucket (Maybe Text)
cbGrantFullControl = lens _cbGrantFullControl (\s a -> s {_cbGrantFullControl = a})

-- | The configuration information for the bucket.
cbCreateBucketConfiguration :: Lens' CreateBucket (Maybe CreateBucketConfiguration)
cbCreateBucketConfiguration = lens _cbCreateBucketConfiguration (\s a -> s {_cbCreateBucketConfiguration = a})

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
cbGrantWrite :: Lens' CreateBucket (Maybe Text)
cbGrantWrite = lens _cbGrantWrite (\s a -> s {_cbGrantWrite = a})

-- | The canned ACL to apply to the bucket.
cbACL :: Lens' CreateBucket (Maybe BucketCannedACL)
cbACL = lens _cbACL (\s a -> s {_cbACL = a})

-- | The name of the bucket to create.
cbBucket :: Lens' CreateBucket BucketName
cbBucket = lens _cbBucket (\s a -> s {_cbBucket = a})

instance AWSRequest CreateBucket where
  type Rs CreateBucket = CreateBucketResponse
  request = putXML s3
  response =
    receiveEmpty
      ( \s h x ->
          CreateBucketResponse'
            <$> (h .#? "Location") <*> (pure (fromEnum s))
      )

instance Hashable CreateBucket

instance NFData CreateBucket

instance ToElement CreateBucket where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
      . _cbCreateBucketConfiguration

instance ToHeaders CreateBucket where
  toHeaders CreateBucket' {..} =
    mconcat
      [ "x-amz-grant-read-acp" =# _cbGrantReadACP,
        "x-amz-bucket-object-lock-enabled"
          =# _cbObjectLockEnabledForBucket,
        "x-amz-grant-write-acp" =# _cbGrantWriteACP,
        "x-amz-grant-read" =# _cbGrantRead,
        "x-amz-grant-full-control" =# _cbGrantFullControl,
        "x-amz-grant-write" =# _cbGrantWrite,
        "x-amz-acl" =# _cbACL
      ]

instance ToPath CreateBucket where
  toPath CreateBucket' {..} = mconcat ["/", toBS _cbBucket]

instance ToQuery CreateBucket where
  toQuery = const mempty

-- | /See:/ 'createBucketResponse' smart constructor.
data CreateBucketResponse = CreateBucketResponse'
  { _cbrsLocation ::
      !(Maybe Text),
    _cbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBucketResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsLocation' - Specifies the Region where the bucket will be created. If you are creating a bucket on the US East (N. Virginia) Region (us-east-1), you do not need to specify the location.
--
-- * 'cbrsResponseStatus' - -- | The response status code.
createBucketResponse ::
  -- | 'cbrsResponseStatus'
  Int ->
  CreateBucketResponse
createBucketResponse pResponseStatus_ =
  CreateBucketResponse'
    { _cbrsLocation = Nothing,
      _cbrsResponseStatus = pResponseStatus_
    }

-- | Specifies the Region where the bucket will be created. If you are creating a bucket on the US East (N. Virginia) Region (us-east-1), you do not need to specify the location.
cbrsLocation :: Lens' CreateBucketResponse (Maybe Text)
cbrsLocation = lens _cbrsLocation (\s a -> s {_cbrsLocation = a})

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBucketResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\s a -> s {_cbrsResponseStatus = a})

instance NFData CreateBucketResponse
