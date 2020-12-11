{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is either running or stopped.
--
-- If you customized your instance with instance store volumes or EBS volumes in addition to the root device volume, the new AMI contains block device mapping information for those volumes. When you launch an instance from this new AMI, the instance automatically launches with those additional volumes.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating Amazon EBS-Backed Linux AMIs> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateImage
  ( -- * Creating a request
    CreateImage (..),
    mkCreateImage,

    -- ** Request lenses
    ciiNoReboot,
    ciiDescription,
    ciiBlockDeviceMappings,
    ciiDryRun,
    ciiInstanceId,
    ciiName,

    -- * Destructuring the response
    CreateImageResponse (..),
    mkCreateImageResponse,

    -- ** Response lenses
    cirsImageId,
    cirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateImage' smart constructor.
data CreateImage = CreateImage'
  { noReboot :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping],
    dryRun :: Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImage' with the minimum fields required to make a request.
--
-- * 'blockDeviceMappings' - The block device mappings. This parameter cannot be used to modify the encryption status of existing volumes or snapshots. To create an AMI with encrypted snapshots, use the 'CopyImage' action.
-- * 'description' - A description for the new image.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceId' - The ID of the instance.
-- * 'name' - A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
-- * 'noReboot' - By default, Amazon EC2 attempts to shut down and reboot the instance before creating the image. If the 'No Reboot' option is set, Amazon EC2 doesn't shut down the instance before creating the image. When this option is used, file system integrity on the created image can't be guaranteed.
mkCreateImage ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateImage
mkCreateImage pInstanceId_ pName_ =
  CreateImage'
    { noReboot = Lude.Nothing,
      description = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      dryRun = Lude.Nothing,
      instanceId = pInstanceId_,
      name = pName_
    }

-- | By default, Amazon EC2 attempts to shut down and reboot the instance before creating the image. If the 'No Reboot' option is set, Amazon EC2 doesn't shut down the instance before creating the image. When this option is used, file system integrity on the created image can't be guaranteed.
--
-- /Note:/ Consider using 'noReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciiNoReboot :: Lens.Lens' CreateImage (Lude.Maybe Lude.Bool)
ciiNoReboot = Lens.lens (noReboot :: CreateImage -> Lude.Maybe Lude.Bool) (\s a -> s {noReboot = a} :: CreateImage)
{-# DEPRECATED ciiNoReboot "Use generic-lens or generic-optics with 'noReboot' instead." #-}

-- | A description for the new image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciiDescription :: Lens.Lens' CreateImage (Lude.Maybe Lude.Text)
ciiDescription = Lens.lens (description :: CreateImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateImage)
{-# DEPRECATED ciiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The block device mappings. This parameter cannot be used to modify the encryption status of existing volumes or snapshots. To create an AMI with encrypted snapshots, use the 'CopyImage' action.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciiBlockDeviceMappings :: Lens.Lens' CreateImage (Lude.Maybe [BlockDeviceMapping])
ciiBlockDeviceMappings = Lens.lens (blockDeviceMappings :: CreateImage -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: CreateImage)
{-# DEPRECATED ciiBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciiDryRun :: Lens.Lens' CreateImage (Lude.Maybe Lude.Bool)
ciiDryRun = Lens.lens (dryRun :: CreateImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateImage)
{-# DEPRECATED ciiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciiInstanceId :: Lens.Lens' CreateImage Lude.Text
ciiInstanceId = Lens.lens (instanceId :: CreateImage -> Lude.Text) (\s a -> s {instanceId = a} :: CreateImage)
{-# DEPRECATED ciiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciiName :: Lens.Lens' CreateImage Lude.Text
ciiName = Lens.lens (name :: CreateImage -> Lude.Text) (\s a -> s {name = a} :: CreateImage)
{-# DEPRECATED ciiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateImage where
  type Rs CreateImage = CreateImageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateImageResponse'
            Lude.<$> (x Lude..@? "imageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateImage where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateImage where
  toQuery CreateImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NoReboot" Lude.=: noReboot,
        "Description" Lude.=: description,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId,
        "Name" Lude.=: name
      ]

-- | /See:/ 'mkCreateImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { imageId ::
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

-- | Creates a value of 'CreateImageResponse' with the minimum fields required to make a request.
--
-- * 'imageId' - The ID of the new AMI.
-- * 'responseStatus' - The response status code.
mkCreateImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateImageResponse
mkCreateImageResponse pResponseStatus_ =
  CreateImageResponse'
    { imageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the new AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsImageId :: Lens.Lens' CreateImageResponse (Lude.Maybe Lude.Text)
cirsImageId = Lens.lens (imageId :: CreateImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: CreateImageResponse)
{-# DEPRECATED cirsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateImageResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateImageResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
