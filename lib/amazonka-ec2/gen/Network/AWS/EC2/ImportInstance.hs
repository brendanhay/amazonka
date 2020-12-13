{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import instance task using metadata from the specified disk image. @ImportInstance@ only supports single-volume VMs. To import multi-volume VMs, use 'ImportImage' . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html Importing a Virtual Machine Using the Amazon EC2 CLI> .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
module Network.AWS.EC2.ImportInstance
  ( -- * Creating a request
    ImportInstance (..),
    mkImportInstance,

    -- ** Request lenses
    iiPlatform,
    iiLaunchSpecification,
    iiDiskImages,
    iiDescription,
    iiDryRun,

    -- * Destructuring the response
    ImportInstanceResponse (..),
    mkImportInstanceResponse,

    -- ** Response lenses
    iirsConversionTask,
    iirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportInstance' smart constructor.
data ImportInstance = ImportInstance'
  { -- | The instance operating system.
    platform :: PlatformValues,
    -- | The launch specification.
    launchSpecification :: Lude.Maybe ImportInstanceLaunchSpecification,
    -- | The disk image.
    diskImages :: Lude.Maybe [DiskImage],
    -- | A description for the instance being imported.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportInstance' with the minimum fields required to make a request.
--
-- * 'platform' - The instance operating system.
-- * 'launchSpecification' - The launch specification.
-- * 'diskImages' - The disk image.
-- * 'description' - A description for the instance being imported.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkImportInstance ::
  -- | 'platform'
  PlatformValues ->
  ImportInstance
mkImportInstance pPlatform_ =
  ImportInstance'
    { platform = pPlatform_,
      launchSpecification = Lude.Nothing,
      diskImages = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The instance operating system.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatform :: Lens.Lens' ImportInstance PlatformValues
iiPlatform = Lens.lens (platform :: ImportInstance -> PlatformValues) (\s a -> s {platform = a} :: ImportInstance)
{-# DEPRECATED iiPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The launch specification.
--
-- /Note:/ Consider using 'launchSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLaunchSpecification :: Lens.Lens' ImportInstance (Lude.Maybe ImportInstanceLaunchSpecification)
iiLaunchSpecification = Lens.lens (launchSpecification :: ImportInstance -> Lude.Maybe ImportInstanceLaunchSpecification) (\s a -> s {launchSpecification = a} :: ImportInstance)
{-# DEPRECATED iiLaunchSpecification "Use generic-lens or generic-optics with 'launchSpecification' instead." #-}

-- | The disk image.
--
-- /Note:/ Consider using 'diskImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDiskImages :: Lens.Lens' ImportInstance (Lude.Maybe [DiskImage])
iiDiskImages = Lens.lens (diskImages :: ImportInstance -> Lude.Maybe [DiskImage]) (\s a -> s {diskImages = a} :: ImportInstance)
{-# DEPRECATED iiDiskImages "Use generic-lens or generic-optics with 'diskImages' instead." #-}

-- | A description for the instance being imported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDescription :: Lens.Lens' ImportInstance (Lude.Maybe Lude.Text)
iiDescription = Lens.lens (description :: ImportInstance -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportInstance)
{-# DEPRECATED iiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDryRun :: Lens.Lens' ImportInstance (Lude.Maybe Lude.Bool)
iiDryRun = Lens.lens (dryRun :: ImportInstance -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportInstance)
{-# DEPRECATED iiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ImportInstance where
  type Rs ImportInstance = ImportInstanceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ImportInstanceResponse'
            Lude.<$> (x Lude..@? "conversionTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ImportInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportInstance where
  toQuery ImportInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ImportInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Platform" Lude.=: platform,
        "LaunchSpecification" Lude.=: launchSpecification,
        Lude.toQuery (Lude.toQueryList "DiskImage" Lude.<$> diskImages),
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkImportInstanceResponse' smart constructor.
data ImportInstanceResponse = ImportInstanceResponse'
  { -- | Information about the conversion task.
    conversionTask :: Lude.Maybe ConversionTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportInstanceResponse' with the minimum fields required to make a request.
--
-- * 'conversionTask' - Information about the conversion task.
-- * 'responseStatus' - The response status code.
mkImportInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportInstanceResponse
mkImportInstanceResponse pResponseStatus_ =
  ImportInstanceResponse'
    { conversionTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the conversion task.
--
-- /Note:/ Consider using 'conversionTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iirsConversionTask :: Lens.Lens' ImportInstanceResponse (Lude.Maybe ConversionTask)
iirsConversionTask = Lens.lens (conversionTask :: ImportInstanceResponse -> Lude.Maybe ConversionTask) (\s a -> s {conversionTask = a} :: ImportInstanceResponse)
{-# DEPRECATED iirsConversionTask "Use generic-lens or generic-optics with 'conversionTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iirsResponseStatus :: Lens.Lens' ImportInstanceResponse Lude.Int
iirsResponseStatus = Lens.lens (responseStatus :: ImportInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportInstanceResponse)
{-# DEPRECATED iirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
