{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateStorageLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bucket in Amazon S3 to store application versions, logs, and other files used by Elastic Beanstalk environments. The Elastic Beanstalk console and EB CLI call this API the first time you create an environment in a region. If the storage location already exists, @CreateStorageLocation@ still returns the bucket name but does not create a new bucket.
module Network.AWS.ElasticBeanstalk.CreateStorageLocation
  ( -- * Creating a request
    CreateStorageLocation (..),
    mkCreateStorageLocation,

    -- * Destructuring the response
    CreateStorageLocationResponse (..),
    mkCreateStorageLocationResponse,

    -- ** Response lenses
    cslrsS3Bucket,
    cslrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStorageLocation' smart constructor.
data CreateStorageLocation = CreateStorageLocation'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStorageLocation' with the minimum fields required to make a request.
mkCreateStorageLocation ::
  CreateStorageLocation
mkCreateStorageLocation = CreateStorageLocation'

instance Lude.AWSRequest CreateStorageLocation where
  type Rs CreateStorageLocation = CreateStorageLocationResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "CreateStorageLocationResult"
      ( \s h x ->
          CreateStorageLocationResponse'
            Lude.<$> (x Lude..@? "S3Bucket") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStorageLocation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateStorageLocation where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStorageLocation where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("CreateStorageLocation" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | Results of a 'CreateStorageLocationResult' call.
--
-- /See:/ 'mkCreateStorageLocationResponse' smart constructor.
data CreateStorageLocationResponse = CreateStorageLocationResponse'
  { -- | The name of the Amazon S3 bucket created.
    s3Bucket :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStorageLocationResponse' with the minimum fields required to make a request.
--
-- * 's3Bucket' - The name of the Amazon S3 bucket created.
-- * 'responseStatus' - The response status code.
mkCreateStorageLocationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStorageLocationResponse
mkCreateStorageLocationResponse pResponseStatus_ =
  CreateStorageLocationResponse'
    { s3Bucket = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the Amazon S3 bucket created.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrsS3Bucket :: Lens.Lens' CreateStorageLocationResponse (Lude.Maybe Lude.Text)
cslrsS3Bucket = Lens.lens (s3Bucket :: CreateStorageLocationResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: CreateStorageLocationResponse)
{-# DEPRECATED cslrsS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrsResponseStatus :: Lens.Lens' CreateStorageLocationResponse Lude.Int
cslrsResponseStatus = Lens.lens (responseStatus :: CreateStorageLocationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStorageLocationResponse)
{-# DEPRECATED cslrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
