{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Not every string is an acceptable bucket name. For information about bucket naming restrictions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html Working with Amazon S3 buckets> .
-- If you want to create an Amazon S3 on Outposts bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_control_CreateBucket.html Create Bucket> .
-- By default, the bucket is created in the US East (N. Virginia) Region. You can optionally specify a Region in the request body. You might choose a Region to optimize latency, minimize costs, or address regulatory requirements. For example, if you reside in Europe, you will probably find it advantageous to create buckets in the Europe (Ireland) Region. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html#access-bucket-intro Accessing a bucket> .
-- When creating a bucket using this operation, you can optionally specify the accounts or groups that should be granted specific permissions on the bucket. There are two ways to grant the appropriate permissions using the request headers.
--
--     * Specify a canned ACL using the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly using the @x-amz-grant-read@ , @x-amz-grant-write@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. These headers map to the set of permissions Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access control list (ACL) overview> .
-- You specify each grantee as a type=value pair, where the type is one of the following:
--
--     * @id@ – if the value specified is the canonical user ID of an AWS account
--
--
--     * @uri@ – if you are granting permissions to a predefined group
--
--
--     * @emailAddress@ – if the value specified is the email address of an AWS account
--
--
-- For example, the following @x-amz-grant-read@ header grants the AWS accounts identified by account IDs permissions to read object data and its metadata:
-- @x-amz-grant-read: id="11112222333", id="444455556666" @
--
--
-- The following operations are related to @CreateBucket@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Network.AWS.S3.CreateBucket
  ( -- * Creating a request
    CreateBucket (..),
    mkCreateBucket,

    -- ** Request lenses
    cbGrantReadACP,
    cbObjectLockEnabledForBucket,
    cbGrantWriteACP,
    cbGrantRead,
    cbGrantFullControl,
    cbCreateBucketConfiguration,
    cbGrantWrite,
    cbACL,
    cbBucket,

    -- * Destructuring the response
    CreateBucketResponse (..),
    mkCreateBucketResponse,

    -- ** Response lenses
    cbrsLocation,
    cbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkCreateBucket' smart constructor.
data CreateBucket = CreateBucket'
  { grantReadACP ::
      Lude.Maybe Lude.Text,
    objectLockEnabledForBucket :: Lude.Maybe Lude.Bool,
    grantWriteACP :: Lude.Maybe Lude.Text,
    grantRead :: Lude.Maybe Lude.Text,
    grantFullControl :: Lude.Maybe Lude.Text,
    createBucketConfiguration :: Lude.Maybe CreateBucketConfiguration,
    grantWrite :: Lude.Maybe Lude.Text,
    acl :: Lude.Maybe BucketCannedACL,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBucket' with the minimum fields required to make a request.
--
-- * 'acl' - The canned ACL to apply to the bucket.
-- * 'bucket' - The name of the bucket to create.
-- * 'createBucketConfiguration' - The configuration information for the bucket.
-- * 'grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
-- * 'grantRead' - Allows grantee to list the objects in the bucket.
-- * 'grantReadACP' - Allows grantee to read the bucket ACL.
-- * 'grantWrite' - Allows grantee to create, overwrite, and delete any object in the bucket.
-- * 'grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
-- * 'objectLockEnabledForBucket' - Specifies whether you want S3 Object Lock to be enabled for the new bucket.
mkCreateBucket ::
  -- | 'bucket'
  BucketName ->
  CreateBucket
mkCreateBucket pBucket_ =
  CreateBucket'
    { grantReadACP = Lude.Nothing,
      objectLockEnabledForBucket = Lude.Nothing,
      grantWriteACP = Lude.Nothing,
      grantRead = Lude.Nothing,
      grantFullControl = Lude.Nothing,
      createBucketConfiguration = Lude.Nothing,
      grantWrite = Lude.Nothing,
      acl = Lude.Nothing,
      bucket = pBucket_
    }

-- | Allows grantee to read the bucket ACL.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantReadACP :: Lens.Lens' CreateBucket (Lude.Maybe Lude.Text)
cbGrantReadACP = Lens.lens (grantReadACP :: CreateBucket -> Lude.Maybe Lude.Text) (\s a -> s {grantReadACP = a} :: CreateBucket)
{-# DEPRECATED cbGrantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead." #-}

-- | Specifies whether you want S3 Object Lock to be enabled for the new bucket.
--
-- /Note:/ Consider using 'objectLockEnabledForBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbObjectLockEnabledForBucket :: Lens.Lens' CreateBucket (Lude.Maybe Lude.Bool)
cbObjectLockEnabledForBucket = Lens.lens (objectLockEnabledForBucket :: CreateBucket -> Lude.Maybe Lude.Bool) (\s a -> s {objectLockEnabledForBucket = a} :: CreateBucket)
{-# DEPRECATED cbObjectLockEnabledForBucket "Use generic-lens or generic-optics with 'objectLockEnabledForBucket' instead." #-}

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantWriteACP :: Lens.Lens' CreateBucket (Lude.Maybe Lude.Text)
cbGrantWriteACP = Lens.lens (grantWriteACP :: CreateBucket -> Lude.Maybe Lude.Text) (\s a -> s {grantWriteACP = a} :: CreateBucket)
{-# DEPRECATED cbGrantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead." #-}

-- | Allows grantee to list the objects in the bucket.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantRead :: Lens.Lens' CreateBucket (Lude.Maybe Lude.Text)
cbGrantRead = Lens.lens (grantRead :: CreateBucket -> Lude.Maybe Lude.Text) (\s a -> s {grantRead = a} :: CreateBucket)
{-# DEPRECATED cbGrantRead "Use generic-lens or generic-optics with 'grantRead' instead." #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantFullControl :: Lens.Lens' CreateBucket (Lude.Maybe Lude.Text)
cbGrantFullControl = Lens.lens (grantFullControl :: CreateBucket -> Lude.Maybe Lude.Text) (\s a -> s {grantFullControl = a} :: CreateBucket)
{-# DEPRECATED cbGrantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead." #-}

-- | The configuration information for the bucket.
--
-- /Note:/ Consider using 'createBucketConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCreateBucketConfiguration :: Lens.Lens' CreateBucket (Lude.Maybe CreateBucketConfiguration)
cbCreateBucketConfiguration = Lens.lens (createBucketConfiguration :: CreateBucket -> Lude.Maybe CreateBucketConfiguration) (\s a -> s {createBucketConfiguration = a} :: CreateBucket)
{-# DEPRECATED cbCreateBucketConfiguration "Use generic-lens or generic-optics with 'createBucketConfiguration' instead." #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- /Note:/ Consider using 'grantWrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantWrite :: Lens.Lens' CreateBucket (Lude.Maybe Lude.Text)
cbGrantWrite = Lens.lens (grantWrite :: CreateBucket -> Lude.Maybe Lude.Text) (\s a -> s {grantWrite = a} :: CreateBucket)
{-# DEPRECATED cbGrantWrite "Use generic-lens or generic-optics with 'grantWrite' instead." #-}

-- | The canned ACL to apply to the bucket.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbACL :: Lens.Lens' CreateBucket (Lude.Maybe BucketCannedACL)
cbACL = Lens.lens (acl :: CreateBucket -> Lude.Maybe BucketCannedACL) (\s a -> s {acl = a} :: CreateBucket)
{-# DEPRECATED cbACL "Use generic-lens or generic-optics with 'acl' instead." #-}

-- | The name of the bucket to create.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBucket :: Lens.Lens' CreateBucket BucketName
cbBucket = Lens.lens (bucket :: CreateBucket -> BucketName) (\s a -> s {bucket = a} :: CreateBucket)
{-# DEPRECATED cbBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest CreateBucket where
  type Rs CreateBucket = CreateBucketResponse
  request = Req.putXML s3Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateBucketResponse'
            Lude.<$> (h Lude..#? "Location") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateBucket where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
      Lude.. createBucketConfiguration

instance Lude.ToHeaders CreateBucket where
  toHeaders CreateBucket' {..} =
    Lude.mconcat
      [ "x-amz-grant-read-acp" Lude.=# grantReadACP,
        "x-amz-bucket-object-lock-enabled"
          Lude.=# objectLockEnabledForBucket,
        "x-amz-grant-write-acp" Lude.=# grantWriteACP,
        "x-amz-grant-read" Lude.=# grantRead,
        "x-amz-grant-full-control" Lude.=# grantFullControl,
        "x-amz-grant-write" Lude.=# grantWrite,
        "x-amz-acl" Lude.=# acl
      ]

instance Lude.ToPath CreateBucket where
  toPath CreateBucket' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery CreateBucket where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBucketResponse' smart constructor.
data CreateBucketResponse = CreateBucketResponse'
  { location ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBucketResponse' with the minimum fields required to make a request.
--
-- * 'location' - Specifies the Region where the bucket will be created. If you are creating a bucket on the US East (N. Virginia) Region (us-east-1), you do not need to specify the location.
-- * 'responseStatus' - The response status code.
mkCreateBucketResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBucketResponse
mkCreateBucketResponse pResponseStatus_ =
  CreateBucketResponse'
    { location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Specifies the Region where the bucket will be created. If you are creating a bucket on the US East (N. Virginia) Region (us-east-1), you do not need to specify the location.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsLocation :: Lens.Lens' CreateBucketResponse (Lude.Maybe Lude.Text)
cbrsLocation = Lens.lens (location :: CreateBucketResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateBucketResponse)
{-# DEPRECATED cbrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsResponseStatus :: Lens.Lens' CreateBucketResponse Lude.Int
cbrsResponseStatus = Lens.lens (responseStatus :: CreateBucketResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBucketResponse)
{-# DEPRECATED cbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
