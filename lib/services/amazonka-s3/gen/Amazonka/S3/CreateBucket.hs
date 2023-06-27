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
-- Module      : Amazonka.S3.CreateBucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new S3 bucket. To create a bucket, you must register with
-- Amazon S3 and have a valid Amazon Web Services Access Key ID to
-- authenticate requests. Anonymous requests are never allowed to create
-- buckets. By creating the bucket, you become the bucket owner.
--
-- Not every string is an acceptable bucket name. For information about
-- bucket naming restrictions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html Bucket naming rules>.
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
-- endpoint, the request goes to the @us-east-1@ Region. Accordingly, the
-- signature calculations in Signature Version 4 must use @us-east-1@ as
-- the Region, even if the location constraint in the request specifies
-- another Region where the bucket is to be created. If you create a bucket
-- in a Region other than US East (N. Virginia), your application must be
-- able to handle 307 redirect. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html Virtual hosting of buckets>.
--
-- [Permissions]
--     In addition to @s3:CreateBucket@, the following permissions are
--     required when your @CreateBucket@ request includes specific headers:
--
--     -   __Access control lists (ACLs)__ - If your @CreateBucket@ request
--         specifies access control list (ACL) permissions and the ACL is
--         public-read, public-read-write, authenticated-read, or if you
--         specify access permissions explicitly through any other ACL,
--         both @s3:CreateBucket@ and @s3:PutBucketAcl@ permissions are
--         needed. If the ACL for the @CreateBucket@ request is private or
--         if the request doesn\'t specify any ACLs, only @s3:CreateBucket@
--         permission is needed.
--
--     -   __Object Lock__ - If @ObjectLockEnabledForBucket@ is set to true
--         in your @CreateBucket@ request,
--         @s3:PutBucketObjectLockConfiguration@ and
--         @s3:PutBucketVersioning@ permissions are required.
--
--     -   __S3 Object Ownership__ - If your @CreateBucket@ request
--         includes the @x-amz-object-ownership@ header, then the
--         @s3:PutBucketOwnershipControls@ permission is required. By
--         default, @ObjectOwnership@ is set to @BucketOWnerEnforced@ and
--         ACLs are disabled. We recommend keeping ACLs disabled, except in
--         uncommon use cases where you must control access for each object
--         individually. If you want to change the @ObjectOwnership@
--         setting, you can use the @x-amz-object-ownership@ header in your
--         @CreateBucket@ request to set the @ObjectOwnership@ setting of
--         your choice. For more information about S3 Object Ownership, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling object ownership>
--         in the /Amazon S3 User Guide/.
--
--     -   __S3 Block Public Access__ - If your specific use case requires
--         granting public access to your S3 resources, you can disable
--         Block Public Access. You can create a new bucket with Block
--         Public Access enabled, then separately call the
--         <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
--         API. To use this operation, you must have the
--         @s3:PutBucketPublicAccessBlock@ permission. By default, all
--         Block Public Access settings are enabled for new buckets. To
--         avoid inadvertent exposure of your resources, we recommend
--         keeping the S3 Block Public Access settings enabled. For more
--         information about S3 Block Public Access, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Blocking public access to your Amazon S3 storage>
--         in the /Amazon S3 User Guide/.
--
-- If your @CreateBucket@ request sets @BucketOwnerEnforced@ for Amazon S3
-- Object Ownership and specifies a bucket ACL that provides access to an
-- external Amazon Web Services account, your request fails with a @400@
-- error and returns the @InvalidBucketAcLWithObjectOwnership@ error code.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-ownership-existing-bucket.html Setting Object Ownership on an existing bucket>
-- in the /Amazon S3 User Guide/.
--
-- The following operations are related to @CreateBucket@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Amazonka.S3.CreateBucket
  ( -- * Creating a Request
    CreateBucket (..),
    newCreateBucket,

    -- * Request Lenses
    createBucket_acl,
    createBucket_createBucketConfiguration,
    createBucket_grantFullControl,
    createBucket_grantRead,
    createBucket_grantReadACP,
    createBucket_grantWrite,
    createBucket_grantWriteACP,
    createBucket_objectLockEnabledForBucket,
    createBucket_objectOwnership,
    createBucket_bucket,

    -- * Destructuring the Response
    CreateBucketResponse (..),
    newCreateBucketResponse,

    -- * Response Lenses
    createBucketResponse_location,
    createBucketResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newCreateBucket' smart constructor.
data CreateBucket = CreateBucket'
  { -- | The canned ACL to apply to the bucket.
    acl :: Prelude.Maybe BucketCannedACL,
    -- | The configuration information for the bucket.
    createBucketConfiguration :: Prelude.Maybe CreateBucketConfiguration,
    -- | Allows grantee the read, write, read ACP, and write ACP permissions on
    -- the bucket.
    grantFullControl :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to list the objects in the bucket.
    grantRead :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to read the bucket ACL.
    grantReadACP :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to create new objects in the bucket.
    --
    -- For the bucket and object owners of existing objects, also allows
    -- deletions and overwrites of those objects.
    grantWrite :: Prelude.Maybe Prelude.Text,
    -- | Allows grantee to write the ACL for the applicable bucket.
    grantWriteACP :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether you want S3 Object Lock to be enabled for the new
    -- bucket.
    objectLockEnabledForBucket :: Prelude.Maybe Prelude.Bool,
    objectOwnership :: Prelude.Maybe ObjectOwnership,
    -- | The name of the bucket to create.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acl', 'createBucket_acl' - The canned ACL to apply to the bucket.
--
-- 'createBucketConfiguration', 'createBucket_createBucketConfiguration' - The configuration information for the bucket.
--
-- 'grantFullControl', 'createBucket_grantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
--
-- 'grantRead', 'createBucket_grantRead' - Allows grantee to list the objects in the bucket.
--
-- 'grantReadACP', 'createBucket_grantReadACP' - Allows grantee to read the bucket ACL.
--
-- 'grantWrite', 'createBucket_grantWrite' - Allows grantee to create new objects in the bucket.
--
-- For the bucket and object owners of existing objects, also allows
-- deletions and overwrites of those objects.
--
-- 'grantWriteACP', 'createBucket_grantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- 'objectLockEnabledForBucket', 'createBucket_objectLockEnabledForBucket' - Specifies whether you want S3 Object Lock to be enabled for the new
-- bucket.
--
-- 'objectOwnership', 'createBucket_objectOwnership' - Undocumented member.
--
-- 'bucket', 'createBucket_bucket' - The name of the bucket to create.
newCreateBucket ::
  -- | 'bucket'
  BucketName ->
  CreateBucket
newCreateBucket pBucket_ =
  CreateBucket'
    { acl = Prelude.Nothing,
      createBucketConfiguration = Prelude.Nothing,
      grantFullControl = Prelude.Nothing,
      grantRead = Prelude.Nothing,
      grantReadACP = Prelude.Nothing,
      grantWrite = Prelude.Nothing,
      grantWriteACP = Prelude.Nothing,
      objectLockEnabledForBucket = Prelude.Nothing,
      objectOwnership = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The canned ACL to apply to the bucket.
createBucket_acl :: Lens.Lens' CreateBucket (Prelude.Maybe BucketCannedACL)
createBucket_acl = Lens.lens (\CreateBucket' {acl} -> acl) (\s@CreateBucket' {} a -> s {acl = a} :: CreateBucket)

-- | The configuration information for the bucket.
createBucket_createBucketConfiguration :: Lens.Lens' CreateBucket (Prelude.Maybe CreateBucketConfiguration)
createBucket_createBucketConfiguration = Lens.lens (\CreateBucket' {createBucketConfiguration} -> createBucketConfiguration) (\s@CreateBucket' {} a -> s {createBucketConfiguration = a} :: CreateBucket)

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
createBucket_grantFullControl :: Lens.Lens' CreateBucket (Prelude.Maybe Prelude.Text)
createBucket_grantFullControl = Lens.lens (\CreateBucket' {grantFullControl} -> grantFullControl) (\s@CreateBucket' {} a -> s {grantFullControl = a} :: CreateBucket)

-- | Allows grantee to list the objects in the bucket.
createBucket_grantRead :: Lens.Lens' CreateBucket (Prelude.Maybe Prelude.Text)
createBucket_grantRead = Lens.lens (\CreateBucket' {grantRead} -> grantRead) (\s@CreateBucket' {} a -> s {grantRead = a} :: CreateBucket)

-- | Allows grantee to read the bucket ACL.
createBucket_grantReadACP :: Lens.Lens' CreateBucket (Prelude.Maybe Prelude.Text)
createBucket_grantReadACP = Lens.lens (\CreateBucket' {grantReadACP} -> grantReadACP) (\s@CreateBucket' {} a -> s {grantReadACP = a} :: CreateBucket)

-- | Allows grantee to create new objects in the bucket.
--
-- For the bucket and object owners of existing objects, also allows
-- deletions and overwrites of those objects.
createBucket_grantWrite :: Lens.Lens' CreateBucket (Prelude.Maybe Prelude.Text)
createBucket_grantWrite = Lens.lens (\CreateBucket' {grantWrite} -> grantWrite) (\s@CreateBucket' {} a -> s {grantWrite = a} :: CreateBucket)

-- | Allows grantee to write the ACL for the applicable bucket.
createBucket_grantWriteACP :: Lens.Lens' CreateBucket (Prelude.Maybe Prelude.Text)
createBucket_grantWriteACP = Lens.lens (\CreateBucket' {grantWriteACP} -> grantWriteACP) (\s@CreateBucket' {} a -> s {grantWriteACP = a} :: CreateBucket)

-- | Specifies whether you want S3 Object Lock to be enabled for the new
-- bucket.
createBucket_objectLockEnabledForBucket :: Lens.Lens' CreateBucket (Prelude.Maybe Prelude.Bool)
createBucket_objectLockEnabledForBucket = Lens.lens (\CreateBucket' {objectLockEnabledForBucket} -> objectLockEnabledForBucket) (\s@CreateBucket' {} a -> s {objectLockEnabledForBucket = a} :: CreateBucket)

-- | Undocumented member.
createBucket_objectOwnership :: Lens.Lens' CreateBucket (Prelude.Maybe ObjectOwnership)
createBucket_objectOwnership = Lens.lens (\CreateBucket' {objectOwnership} -> objectOwnership) (\s@CreateBucket' {} a -> s {objectOwnership = a} :: CreateBucket)

-- | The name of the bucket to create.
createBucket_bucket :: Lens.Lens' CreateBucket BucketName
createBucket_bucket = Lens.lens (\CreateBucket' {bucket} -> bucket) (\s@CreateBucket' {} a -> s {bucket = a} :: CreateBucket)

instance Core.AWSRequest CreateBucket where
  type AWSResponse CreateBucket = CreateBucketResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateBucketResponse'
            Prelude.<$> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBucket where
  hashWithSalt _salt CreateBucket' {..} =
    _salt
      `Prelude.hashWithSalt` acl
      `Prelude.hashWithSalt` createBucketConfiguration
      `Prelude.hashWithSalt` grantFullControl
      `Prelude.hashWithSalt` grantRead
      `Prelude.hashWithSalt` grantReadACP
      `Prelude.hashWithSalt` grantWrite
      `Prelude.hashWithSalt` grantWriteACP
      `Prelude.hashWithSalt` objectLockEnabledForBucket
      `Prelude.hashWithSalt` objectOwnership
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData CreateBucket where
  rnf CreateBucket' {..} =
    Prelude.rnf acl
      `Prelude.seq` Prelude.rnf createBucketConfiguration
      `Prelude.seq` Prelude.rnf grantFullControl
      `Prelude.seq` Prelude.rnf grantRead
      `Prelude.seq` Prelude.rnf grantReadACP
      `Prelude.seq` Prelude.rnf grantWrite
      `Prelude.seq` Prelude.rnf grantWriteACP
      `Prelude.seq` Prelude.rnf objectLockEnabledForBucket
      `Prelude.seq` Prelude.rnf objectOwnership
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToElement CreateBucket where
  toElement CreateBucket' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
      createBucketConfiguration

instance Data.ToHeaders CreateBucket where
  toHeaders CreateBucket' {..} =
    Prelude.mconcat
      [ "x-amz-acl" Data.=# acl,
        "x-amz-grant-full-control" Data.=# grantFullControl,
        "x-amz-grant-read" Data.=# grantRead,
        "x-amz-grant-read-acp" Data.=# grantReadACP,
        "x-amz-grant-write" Data.=# grantWrite,
        "x-amz-grant-write-acp" Data.=# grantWriteACP,
        "x-amz-bucket-object-lock-enabled"
          Data.=# objectLockEnabledForBucket,
        "x-amz-object-ownership" Data.=# objectOwnership
      ]

instance Data.ToPath CreateBucket where
  toPath CreateBucket' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery CreateBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBucketResponse' smart constructor.
data CreateBucketResponse = CreateBucketResponse'
  { -- | A forward slash followed by the name of the bucket.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'createBucketResponse_location' - A forward slash followed by the name of the bucket.
--
-- 'httpStatus', 'createBucketResponse_httpStatus' - The response's http status code.
newCreateBucketResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBucketResponse
newCreateBucketResponse pHttpStatus_ =
  CreateBucketResponse'
    { location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A forward slash followed by the name of the bucket.
createBucketResponse_location :: Lens.Lens' CreateBucketResponse (Prelude.Maybe Prelude.Text)
createBucketResponse_location = Lens.lens (\CreateBucketResponse' {location} -> location) (\s@CreateBucketResponse' {} a -> s {location = a} :: CreateBucketResponse)

-- | The response's http status code.
createBucketResponse_httpStatus :: Lens.Lens' CreateBucketResponse Prelude.Int
createBucketResponse_httpStatus = Lens.lens (\CreateBucketResponse' {httpStatus} -> httpStatus) (\s@CreateBucketResponse' {} a -> s {httpStatus = a} :: CreateBucketResponse)

instance Prelude.NFData CreateBucketResponse where
  rnf CreateBucketResponse' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
