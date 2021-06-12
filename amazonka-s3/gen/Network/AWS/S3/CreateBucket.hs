{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CreateBucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new S3 bucket. To create a bucket, you must register with
-- Amazon S3 and have a valid AWS Access Key ID to authenticate requests.
-- Anonymous requests are never allowed to create buckets. By creating the
-- bucket, you become the bucket owner.
--
-- Not every string is an acceptable bucket name. For information about
-- bucket naming restrictions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html Working with Amazon S3 buckets>.
--
-- If you want to create an Amazon S3 on Outposts bucket, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_control_CreateBucket.html Create Bucket>.
--
-- By default, the bucket is created in the US East (N. Virginia) Region.
-- You can optionally specify a Region in the request body. You might
-- choose a Region to optimize latency, minimize costs, or address
-- regulatory requirements. For example, if you reside in Europe, you will
-- probably find it advantageous to create buckets in the Europe (Ireland)
-- Region. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html#access-bucket-intro Accessing a bucket>.
--
-- If you send your create bucket request to the @s3.amazonaws.com@
-- endpoint, the request goes to the us-east-1 Region. Accordingly, the
-- signature calculations in Signature Version 4 must use us-east-1 as the
-- Region, even if the location constraint in the request specifies another
-- Region where the bucket is to be created. If you create a bucket in a
-- Region other than US East (N. Virginia), your application must be able
-- to handle 307 redirect. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html Virtual hosting of buckets>.
--
-- When creating a bucket using this operation, you can optionally specify
-- the accounts or groups that should be granted specific permissions on
-- the bucket. There are two ways to grant the appropriate permissions
-- using the request headers.
--
-- -   Specify a canned ACL using the @x-amz-acl@ request header. Amazon S3
--     supports a set of predefined ACLs, known as /canned ACLs/. Each
--     canned ACL has a predefined set of grantees and permissions. For
--     more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL>.
--
-- -   Specify access permissions explicitly using the @x-amz-grant-read@,
--     @x-amz-grant-write@, @x-amz-grant-read-acp@,
--     @x-amz-grant-write-acp@, and @x-amz-grant-full-control@ headers.
--     These headers map to the set of permissions Amazon S3 supports in an
--     ACL. For more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access control list (ACL) overview>.
--
--     You specify each grantee as a type=value pair, where the type is one
--     of the following:
--
--     -   @id@ – if the value specified is the canonical user ID of an AWS
--         account
--
--     -   @uri@ – if you are granting permissions to a predefined group
--
--     -   @emailAddress@ – if the value specified is the email address of
--         an AWS account
--
--         Using email addresses to specify a grantee is only supported in
--         the following AWS Regions:
--
--         -   US East (N. Virginia)
--
--         -   US West (N. California)
--
--         -   US West (Oregon)
--
--         -   Asia Pacific (Singapore)
--
--         -   Asia Pacific (Sydney)
--
--         -   Asia Pacific (Tokyo)
--
--         -   Europe (Ireland)
--
--         -   South America (São Paulo)
--
--         For a list of all the Amazon S3 supported Regions and endpoints,
--         see
--         <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints>
--         in the AWS General Reference.
--
--     For example, the following @x-amz-grant-read@ header grants the AWS
--     accounts identified by account IDs permissions to read object data
--     and its metadata:
--
--     @x-amz-grant-read: id=\"11112222333\", id=\"444455556666\" @
--
-- You can use either a canned ACL or specify access permissions
-- explicitly. You cannot do both.
--
-- The following operations are related to @CreateBucket@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Network.AWS.S3.CreateBucket
  ( -- * Creating a Request
    CreateBucket (..),
    newCreateBucket,

    -- * Request Lenses
    createBucket_grantRead,
    createBucket_createBucketConfiguration,
    createBucket_grantWriteACP,
    createBucket_objectLockEnabledForBucket,
    createBucket_grantReadACP,
    createBucket_acl,
    createBucket_grantWrite,
    createBucket_grantFullControl,
    createBucket_bucket,

    -- * Destructuring the Response
    CreateBucketResponse (..),
    newCreateBucketResponse,

    -- * Response Lenses
    createBucketResponse_location,
    createBucketResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newCreateBucket' smart constructor.
data CreateBucket = CreateBucket'
  { -- | Allows grantee to list the objects in the bucket.
    grantRead :: Core.Maybe Core.Text,
    -- | The configuration information for the bucket.
    createBucketConfiguration :: Core.Maybe CreateBucketConfiguration,
    -- | Allows grantee to write the ACL for the applicable bucket.
    grantWriteACP :: Core.Maybe Core.Text,
    -- | Specifies whether you want S3 Object Lock to be enabled for the new
    -- bucket.
    objectLockEnabledForBucket :: Core.Maybe Core.Bool,
    -- | Allows grantee to read the bucket ACL.
    grantReadACP :: Core.Maybe Core.Text,
    -- | The canned ACL to apply to the bucket.
    acl :: Core.Maybe BucketCannedACL,
    -- | Allows grantee to create, overwrite, and delete any object in the
    -- bucket.
    grantWrite :: Core.Maybe Core.Text,
    -- | Allows grantee the read, write, read ACP, and write ACP permissions on
    -- the bucket.
    grantFullControl :: Core.Maybe Core.Text,
    -- | The name of the bucket to create.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantRead', 'createBucket_grantRead' - Allows grantee to list the objects in the bucket.
--
-- 'createBucketConfiguration', 'createBucket_createBucketConfiguration' - The configuration information for the bucket.
--
-- 'grantWriteACP', 'createBucket_grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- 'objectLockEnabledForBucket', 'createBucket_objectLockEnabledForBucket' - Specifies whether you want S3 Object Lock to be enabled for the new
-- bucket.
--
-- 'grantReadACP', 'createBucket_grantReadACP' - Allows grantee to read the bucket ACL.
--
-- 'acl', 'createBucket_acl' - The canned ACL to apply to the bucket.
--
-- 'grantWrite', 'createBucket_grantWrite' - Allows grantee to create, overwrite, and delete any object in the
-- bucket.
--
-- 'grantFullControl', 'createBucket_grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
--
-- 'bucket', 'createBucket_bucket' - The name of the bucket to create.
newCreateBucket ::
  -- | 'bucket'
  BucketName ->
  CreateBucket
newCreateBucket pBucket_ =
  CreateBucket'
    { grantRead = Core.Nothing,
      createBucketConfiguration = Core.Nothing,
      grantWriteACP = Core.Nothing,
      objectLockEnabledForBucket = Core.Nothing,
      grantReadACP = Core.Nothing,
      acl = Core.Nothing,
      grantWrite = Core.Nothing,
      grantFullControl = Core.Nothing,
      bucket = pBucket_
    }

-- | Allows grantee to list the objects in the bucket.
createBucket_grantRead :: Lens.Lens' CreateBucket (Core.Maybe Core.Text)
createBucket_grantRead = Lens.lens (\CreateBucket' {grantRead} -> grantRead) (\s@CreateBucket' {} a -> s {grantRead = a} :: CreateBucket)

-- | The configuration information for the bucket.
createBucket_createBucketConfiguration :: Lens.Lens' CreateBucket (Core.Maybe CreateBucketConfiguration)
createBucket_createBucketConfiguration = Lens.lens (\CreateBucket' {createBucketConfiguration} -> createBucketConfiguration) (\s@CreateBucket' {} a -> s {createBucketConfiguration = a} :: CreateBucket)

-- | Allows grantee to write the ACL for the applicable bucket.
createBucket_grantWriteACP :: Lens.Lens' CreateBucket (Core.Maybe Core.Text)
createBucket_grantWriteACP = Lens.lens (\CreateBucket' {grantWriteACP} -> grantWriteACP) (\s@CreateBucket' {} a -> s {grantWriteACP = a} :: CreateBucket)

-- | Specifies whether you want S3 Object Lock to be enabled for the new
-- bucket.
createBucket_objectLockEnabledForBucket :: Lens.Lens' CreateBucket (Core.Maybe Core.Bool)
createBucket_objectLockEnabledForBucket = Lens.lens (\CreateBucket' {objectLockEnabledForBucket} -> objectLockEnabledForBucket) (\s@CreateBucket' {} a -> s {objectLockEnabledForBucket = a} :: CreateBucket)

-- | Allows grantee to read the bucket ACL.
createBucket_grantReadACP :: Lens.Lens' CreateBucket (Core.Maybe Core.Text)
createBucket_grantReadACP = Lens.lens (\CreateBucket' {grantReadACP} -> grantReadACP) (\s@CreateBucket' {} a -> s {grantReadACP = a} :: CreateBucket)

-- | The canned ACL to apply to the bucket.
createBucket_acl :: Lens.Lens' CreateBucket (Core.Maybe BucketCannedACL)
createBucket_acl = Lens.lens (\CreateBucket' {acl} -> acl) (\s@CreateBucket' {} a -> s {acl = a} :: CreateBucket)

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
createBucket_grantWrite :: Lens.Lens' CreateBucket (Core.Maybe Core.Text)
createBucket_grantWrite = Lens.lens (\CreateBucket' {grantWrite} -> grantWrite) (\s@CreateBucket' {} a -> s {grantWrite = a} :: CreateBucket)

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
createBucket_grantFullControl :: Lens.Lens' CreateBucket (Core.Maybe Core.Text)
createBucket_grantFullControl = Lens.lens (\CreateBucket' {grantFullControl} -> grantFullControl) (\s@CreateBucket' {} a -> s {grantFullControl = a} :: CreateBucket)

-- | The name of the bucket to create.
createBucket_bucket :: Lens.Lens' CreateBucket BucketName
createBucket_bucket = Lens.lens (\CreateBucket' {bucket} -> bucket) (\s@CreateBucket' {} a -> s {bucket = a} :: CreateBucket)

instance Core.AWSRequest CreateBucket where
  type AWSResponse CreateBucket = CreateBucketResponse
  request = Request.putXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateBucketResponse'
            Core.<$> (h Core..#? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateBucket

instance Core.NFData CreateBucket

instance Core.ToElement CreateBucket where
  toElement CreateBucket' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
      createBucketConfiguration

instance Core.ToHeaders CreateBucket where
  toHeaders CreateBucket' {..} =
    Core.mconcat
      [ "x-amz-grant-read" Core.=# grantRead,
        "x-amz-grant-write-acp" Core.=# grantWriteACP,
        "x-amz-bucket-object-lock-enabled"
          Core.=# objectLockEnabledForBucket,
        "x-amz-grant-read-acp" Core.=# grantReadACP,
        "x-amz-acl" Core.=# acl,
        "x-amz-grant-write" Core.=# grantWrite,
        "x-amz-grant-full-control" Core.=# grantFullControl
      ]

instance Core.ToPath CreateBucket where
  toPath CreateBucket' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery CreateBucket where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateBucketResponse' smart constructor.
data CreateBucketResponse = CreateBucketResponse'
  { -- | Specifies the Region where the bucket will be created. If you are
    -- creating a bucket on the US East (N. Virginia) Region (us-east-1), you
    -- do not need to specify the location.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'createBucketResponse_location' - Specifies the Region where the bucket will be created. If you are
-- creating a bucket on the US East (N. Virginia) Region (us-east-1), you
-- do not need to specify the location.
--
-- 'httpStatus', 'createBucketResponse_httpStatus' - The response's http status code.
newCreateBucketResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateBucketResponse
newCreateBucketResponse pHttpStatus_ =
  CreateBucketResponse'
    { location = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the Region where the bucket will be created. If you are
-- creating a bucket on the US East (N. Virginia) Region (us-east-1), you
-- do not need to specify the location.
createBucketResponse_location :: Lens.Lens' CreateBucketResponse (Core.Maybe Core.Text)
createBucketResponse_location = Lens.lens (\CreateBucketResponse' {location} -> location) (\s@CreateBucketResponse' {} a -> s {location = a} :: CreateBucketResponse)

-- | The response's http status code.
createBucketResponse_httpStatus :: Lens.Lens' CreateBucketResponse Core.Int
createBucketResponse_httpStatus = Lens.lens (\CreateBucketResponse' {httpStatus} -> httpStatus) (\s@CreateBucketResponse' {} a -> s {httpStatus = a} :: CreateBucketResponse)

instance Core.NFData CreateBucketResponse
