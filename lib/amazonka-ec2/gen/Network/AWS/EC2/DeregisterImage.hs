{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified AMI. After you deregister an AMI, it can't be used to launch new instances; however, it doesn't affect any instances that you've already launched from the AMI. You'll continue to incur usage costs for those instances until you terminate them.
--
-- When you deregister an Amazon EBS-backed AMI, it doesn't affect the snapshot that was created for the root volume of the instance during the AMI creation process. When you deregister an instance store-backed AMI, it doesn't affect the files that you uploaded to Amazon S3 when you created the AMI.
module Network.AWS.EC2.DeregisterImage
  ( -- * Creating a request
    DeregisterImage (..),
    mkDeregisterImage,

    -- ** Request lenses
    diDryRun,
    diImageId,

    -- * Destructuring the response
    DeregisterImageResponse (..),
    mkDeregisterImageResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeregisterImage.
--
-- /See:/ 'mkDeregisterImage' smart constructor.
data DeregisterImage = DeregisterImage'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    imageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterImage' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'imageId' - The ID of the AMI.
mkDeregisterImage ::
  -- | 'imageId'
  Lude.Text ->
  DeregisterImage
mkDeregisterImage pImageId_ =
  DeregisterImage' {dryRun = Lude.Nothing, imageId = pImageId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDryRun :: Lens.Lens' DeregisterImage (Lude.Maybe Lude.Bool)
diDryRun = Lens.lens (dryRun :: DeregisterImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeregisterImage)
{-# DEPRECATED diDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageId :: Lens.Lens' DeregisterImage Lude.Text
diImageId = Lens.lens (imageId :: DeregisterImage -> Lude.Text) (\s a -> s {imageId = a} :: DeregisterImage)
{-# DEPRECATED diImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.AWSRequest DeregisterImage where
  type Rs DeregisterImage = DeregisterImageResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeregisterImageResponse'

instance Lude.ToHeaders DeregisterImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterImage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterImage where
  toQuery DeregisterImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeregisterImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "ImageId" Lude.=: imageId
      ]

-- | /See:/ 'mkDeregisterImageResponse' smart constructor.
data DeregisterImageResponse = DeregisterImageResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterImageResponse' with the minimum fields required to make a request.
mkDeregisterImageResponse ::
  DeregisterImageResponse
mkDeregisterImageResponse = DeregisterImageResponse'
