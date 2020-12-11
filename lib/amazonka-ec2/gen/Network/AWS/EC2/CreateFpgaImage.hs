{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateFpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon FPGA Image (AFI) from the specified design checkpoint (DCP).
--
-- The create operation is asynchronous. To verify that the AFI is ready for use, check the output logs.
-- An AFI contains the FPGA bitstream that is ready to download to an FPGA. You can securely deploy an AFI on multiple FPGA-accelerated instances. For more information, see the <https://github.com/aws/aws-fpga/ AWS FPGA Hardware Development Kit> .
module Network.AWS.EC2.CreateFpgaImage
  ( -- * Creating a request
    CreateFpgaImage (..),
    mkCreateFpgaImage,

    -- ** Request lenses
    creClientToken,
    creLogsStorageLocation,
    creTagSpecifications,
    creName,
    creDescription,
    creDryRun,
    creInputStorageLocation,

    -- * Destructuring the response
    CreateFpgaImageResponse (..),
    mkCreateFpgaImageResponse,

    -- ** Response lenses
    cfirsFpgaImageId,
    cfirsFpgaImageGlobalId,
    cfirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFpgaImage' smart constructor.
data CreateFpgaImage = CreateFpgaImage'
  { clientToken ::
      Lude.Maybe Lude.Text,
    logsStorageLocation :: Lude.Maybe StorageLocation,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    name :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    inputStorageLocation :: StorageLocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFpgaImage' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'description' - A description for the AFI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'inputStorageLocation' - The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
-- * 'logsStorageLocation' - The location in Amazon S3 for the output logs.
-- * 'name' - A name for the AFI.
-- * 'tagSpecifications' - The tags to apply to the FPGA image during creation.
mkCreateFpgaImage ::
  -- | 'inputStorageLocation'
  StorageLocation ->
  CreateFpgaImage
mkCreateFpgaImage pInputStorageLocation_ =
  CreateFpgaImage'
    { clientToken = Lude.Nothing,
      logsStorageLocation = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      inputStorageLocation = pInputStorageLocation_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creClientToken :: Lens.Lens' CreateFpgaImage (Lude.Maybe Lude.Text)
creClientToken = Lens.lens (clientToken :: CreateFpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateFpgaImage)
{-# DEPRECATED creClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The location in Amazon S3 for the output logs.
--
-- /Note:/ Consider using 'logsStorageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creLogsStorageLocation :: Lens.Lens' CreateFpgaImage (Lude.Maybe StorageLocation)
creLogsStorageLocation = Lens.lens (logsStorageLocation :: CreateFpgaImage -> Lude.Maybe StorageLocation) (\s a -> s {logsStorageLocation = a} :: CreateFpgaImage)
{-# DEPRECATED creLogsStorageLocation "Use generic-lens or generic-optics with 'logsStorageLocation' instead." #-}

-- | The tags to apply to the FPGA image during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creTagSpecifications :: Lens.Lens' CreateFpgaImage (Lude.Maybe [TagSpecification])
creTagSpecifications = Lens.lens (tagSpecifications :: CreateFpgaImage -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateFpgaImage)
{-# DEPRECATED creTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | A name for the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creName :: Lens.Lens' CreateFpgaImage (Lude.Maybe Lude.Text)
creName = Lens.lens (name :: CreateFpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateFpgaImage)
{-# DEPRECATED creName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description for the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creDescription :: Lens.Lens' CreateFpgaImage (Lude.Maybe Lude.Text)
creDescription = Lens.lens (description :: CreateFpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateFpgaImage)
{-# DEPRECATED creDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creDryRun :: Lens.Lens' CreateFpgaImage (Lude.Maybe Lude.Bool)
creDryRun = Lens.lens (dryRun :: CreateFpgaImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateFpgaImage)
{-# DEPRECATED creDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The location of the encrypted design checkpoint in Amazon S3. The input must be a tarball.
--
-- /Note:/ Consider using 'inputStorageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creInputStorageLocation :: Lens.Lens' CreateFpgaImage StorageLocation
creInputStorageLocation = Lens.lens (inputStorageLocation :: CreateFpgaImage -> StorageLocation) (\s a -> s {inputStorageLocation = a} :: CreateFpgaImage)
{-# DEPRECATED creInputStorageLocation "Use generic-lens or generic-optics with 'inputStorageLocation' instead." #-}

instance Lude.AWSRequest CreateFpgaImage where
  type Rs CreateFpgaImage = CreateFpgaImageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateFpgaImageResponse'
            Lude.<$> (x Lude..@? "fpgaImageId")
            Lude.<*> (x Lude..@? "fpgaImageGlobalId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFpgaImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateFpgaImage where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateFpgaImage where
  toQuery CreateFpgaImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateFpgaImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "LogsStorageLocation" Lude.=: logsStorageLocation,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Name" Lude.=: name,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "InputStorageLocation" Lude.=: inputStorageLocation
      ]

-- | /See:/ 'mkCreateFpgaImageResponse' smart constructor.
data CreateFpgaImageResponse = CreateFpgaImageResponse'
  { fpgaImageId ::
      Lude.Maybe Lude.Text,
    fpgaImageGlobalId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateFpgaImageResponse' with the minimum fields required to make a request.
--
-- * 'fpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
-- * 'fpgaImageId' - The FPGA image identifier (AFI ID).
-- * 'responseStatus' - The response status code.
mkCreateFpgaImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFpgaImageResponse
mkCreateFpgaImageResponse pResponseStatus_ =
  CreateFpgaImageResponse'
    { fpgaImageId = Lude.Nothing,
      fpgaImageGlobalId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The FPGA image identifier (AFI ID).
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfirsFpgaImageId :: Lens.Lens' CreateFpgaImageResponse (Lude.Maybe Lude.Text)
cfirsFpgaImageId = Lens.lens (fpgaImageId :: CreateFpgaImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {fpgaImageId = a} :: CreateFpgaImageResponse)
{-# DEPRECATED cfirsFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The global FPGA image identifier (AGFI ID).
--
-- /Note:/ Consider using 'fpgaImageGlobalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfirsFpgaImageGlobalId :: Lens.Lens' CreateFpgaImageResponse (Lude.Maybe Lude.Text)
cfirsFpgaImageGlobalId = Lens.lens (fpgaImageGlobalId :: CreateFpgaImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {fpgaImageGlobalId = a} :: CreateFpgaImageResponse)
{-# DEPRECATED cfirsFpgaImageGlobalId "Use generic-lens or generic-optics with 'fpgaImageGlobalId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfirsResponseStatus :: Lens.Lens' CreateFpgaImageResponse Lude.Int
cfirsResponseStatus = Lens.lens (responseStatus :: CreateFpgaImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFpgaImageResponse)
{-# DEPRECATED cfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
