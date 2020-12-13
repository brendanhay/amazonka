{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @GET@ operation uses the @acl@ subresource to return the access control list (ACL) of a bucket. To use @GET@ to return the ACL of the bucket, you must have @READ_ACP@ access to the bucket. If @READ_ACP@ permission is granted to the anonymous user, you can return the ACL of the bucket without using an authorization header.
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>
module Network.AWS.S3.GetBucketACL
  ( -- * Creating a request
    GetBucketACL (..),
    mkGetBucketACL,

    -- ** Request lenses
    gbaBucket,
    gbaExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketACLResponse (..),
    mkGetBucketACLResponse,

    -- ** Response lenses
    gbarsGrants,
    gbarsOwner,
    gbarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketACL' smart constructor.
data GetBucketACL = GetBucketACL'
  { -- | Specifies the S3 bucket whose ACL is being requested.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketACL' with the minimum fields required to make a request.
--
-- * 'bucket' - Specifies the S3 bucket whose ACL is being requested.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketACL ::
  -- | 'bucket'
  BucketName ->
  GetBucketACL
mkGetBucketACL pBucket_ =
  GetBucketACL'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | Specifies the S3 bucket whose ACL is being requested.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaBucket :: Lens.Lens' GetBucketACL BucketName
gbaBucket = Lens.lens (bucket :: GetBucketACL -> BucketName) (\s a -> s {bucket = a} :: GetBucketACL)
{-# DEPRECATED gbaBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaExpectedBucketOwner :: Lens.Lens' GetBucketACL (Lude.Maybe Lude.Text)
gbaExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketACL -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketACL)
{-# DEPRECATED gbaExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketACL where
  type Rs GetBucketACL = GetBucketACLResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketACLResponse'
            Lude.<$> ( x Lude..@? "AccessControlList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Grant")
                     )
            Lude.<*> (x Lude..@? "Owner")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketACL where
  toHeaders GetBucketACL' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketACL where
  toPath GetBucketACL' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketACL where
  toQuery = Lude.const (Lude.mconcat ["acl"])

-- | /See:/ 'mkGetBucketACLResponse' smart constructor.
data GetBucketACLResponse = GetBucketACLResponse'
  { -- | A list of grants.
    grants :: Lude.Maybe [Grant],
    -- | Container for the bucket owner's display name and ID.
    owner :: Lude.Maybe Owner,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketACLResponse' with the minimum fields required to make a request.
--
-- * 'grants' - A list of grants.
-- * 'owner' - Container for the bucket owner's display name and ID.
-- * 'responseStatus' - The response status code.
mkGetBucketACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketACLResponse
mkGetBucketACLResponse pResponseStatus_ =
  GetBucketACLResponse'
    { grants = Lude.Nothing,
      owner = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarsGrants :: Lens.Lens' GetBucketACLResponse (Lude.Maybe [Grant])
gbarsGrants = Lens.lens (grants :: GetBucketACLResponse -> Lude.Maybe [Grant]) (\s a -> s {grants = a} :: GetBucketACLResponse)
{-# DEPRECATED gbarsGrants "Use generic-lens or generic-optics with 'grants' instead." #-}

-- | Container for the bucket owner's display name and ID.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarsOwner :: Lens.Lens' GetBucketACLResponse (Lude.Maybe Owner)
gbarsOwner = Lens.lens (owner :: GetBucketACLResponse -> Lude.Maybe Owner) (\s a -> s {owner = a} :: GetBucketACLResponse)
{-# DEPRECATED gbarsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarsResponseStatus :: Lens.Lens' GetBucketACLResponse Lude.Int
gbarsResponseStatus = Lens.lens (responseStatus :: GetBucketACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketACLResponse)
{-# DEPRECATED gbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
