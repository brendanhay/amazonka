{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import volume task using metadata from the specified disk image.For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/importing-your-volumes-into-amazon-ebs.html Importing Disks to Amazon EBS> .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
module Network.AWS.EC2.ImportVolume
  ( -- * Creating a request
    ImportVolume (..),
    mkImportVolume,

    -- ** Request lenses
    ivImage,
    ivVolume,
    ivAvailabilityZone,
    ivDescription,
    ivDryRun,

    -- * Destructuring the response
    ImportVolumeResponse (..),
    mkImportVolumeResponse,

    -- ** Response lenses
    ivrsConversionTask,
    ivrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportVolume' smart constructor.
data ImportVolume = ImportVolume'
  { -- | The disk image.
    image :: DiskImageDetail,
    -- | The volume size.
    volume :: VolumeDetail,
    -- | The Availability Zone for the resulting EBS volume.
    availabilityZone :: Lude.Text,
    -- | A description of the volume.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportVolume' with the minimum fields required to make a request.
--
-- * 'image' - The disk image.
-- * 'volume' - The volume size.
-- * 'availabilityZone' - The Availability Zone for the resulting EBS volume.
-- * 'description' - A description of the volume.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkImportVolume ::
  -- | 'image'
  DiskImageDetail ->
  -- | 'volume'
  VolumeDetail ->
  -- | 'availabilityZone'
  Lude.Text ->
  ImportVolume
mkImportVolume pImage_ pVolume_ pAvailabilityZone_ =
  ImportVolume'
    { image = pImage_,
      volume = pVolume_,
      availabilityZone = pAvailabilityZone_,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The disk image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImage :: Lens.Lens' ImportVolume DiskImageDetail
ivImage = Lens.lens (image :: ImportVolume -> DiskImageDetail) (\s a -> s {image = a} :: ImportVolume)
{-# DEPRECATED ivImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The volume size.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivVolume :: Lens.Lens' ImportVolume VolumeDetail
ivVolume = Lens.lens (volume :: ImportVolume -> VolumeDetail) (\s a -> s {volume = a} :: ImportVolume)
{-# DEPRECATED ivVolume "Use generic-lens or generic-optics with 'volume' instead." #-}

-- | The Availability Zone for the resulting EBS volume.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivAvailabilityZone :: Lens.Lens' ImportVolume Lude.Text
ivAvailabilityZone = Lens.lens (availabilityZone :: ImportVolume -> Lude.Text) (\s a -> s {availabilityZone = a} :: ImportVolume)
{-# DEPRECATED ivAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A description of the volume.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivDescription :: Lens.Lens' ImportVolume (Lude.Maybe Lude.Text)
ivDescription = Lens.lens (description :: ImportVolume -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportVolume)
{-# DEPRECATED ivDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivDryRun :: Lens.Lens' ImportVolume (Lude.Maybe Lude.Bool)
ivDryRun = Lens.lens (dryRun :: ImportVolume -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportVolume)
{-# DEPRECATED ivDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ImportVolume where
  type Rs ImportVolume = ImportVolumeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ImportVolumeResponse'
            Lude.<$> (x Lude..@? "conversionTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportVolume where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ImportVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportVolume where
  toQuery ImportVolume' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ImportVolume" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Image" Lude.=: image,
        "Volume" Lude.=: volume,
        "AvailabilityZone" Lude.=: availabilityZone,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkImportVolumeResponse' smart constructor.
data ImportVolumeResponse = ImportVolumeResponse'
  { -- | Information about the conversion task.
    conversionTask :: Lude.Maybe ConversionTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportVolumeResponse' with the minimum fields required to make a request.
--
-- * 'conversionTask' - Information about the conversion task.
-- * 'responseStatus' - The response status code.
mkImportVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportVolumeResponse
mkImportVolumeResponse pResponseStatus_ =
  ImportVolumeResponse'
    { conversionTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the conversion task.
--
-- /Note:/ Consider using 'conversionTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrsConversionTask :: Lens.Lens' ImportVolumeResponse (Lude.Maybe ConversionTask)
ivrsConversionTask = Lens.lens (conversionTask :: ImportVolumeResponse -> Lude.Maybe ConversionTask) (\s a -> s {conversionTask = a} :: ImportVolumeResponse)
{-# DEPRECATED ivrsConversionTask "Use generic-lens or generic-optics with 'conversionTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrsResponseStatus :: Lens.Lens' ImportVolumeResponse Lude.Int
ivrsResponseStatus = Lens.lens (responseStatus :: ImportVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportVolumeResponse)
{-# DEPRECATED ivrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
