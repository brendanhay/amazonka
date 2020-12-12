{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceImage
  ( WorkspaceImage (..),

    -- * Smart constructor
    mkWorkspaceImage,

    -- * Lenses
    wiState,
    wiOwnerAccountId,
    wiOperatingSystem,
    wiCreated,
    wiRequiredTenancy,
    wiName,
    wiImageId,
    wiErrorCode,
    wiErrorMessage,
    wiDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.OperatingSystem
import Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy
import Network.AWS.WorkSpaces.Types.WorkspaceImageState

-- | Describes a WorkSpace image.
--
-- /See:/ 'mkWorkspaceImage' smart constructor.
data WorkspaceImage = WorkspaceImage'
  { state ::
      Lude.Maybe WorkspaceImageState,
    ownerAccountId :: Lude.Maybe Lude.Text,
    operatingSystem :: Lude.Maybe OperatingSystem,
    created :: Lude.Maybe Lude.Timestamp,
    requiredTenancy :: Lude.Maybe WorkspaceImageRequiredTenancy,
    name :: Lude.Maybe Lude.Text,
    imageId :: Lude.Maybe Lude.Text,
    errorCode :: Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceImage' with the minimum fields required to make a request.
--
-- * 'created' - The date when the image was created. If the image has been shared, the AWS account that the image has been shared with sees the original creation date of the image.
-- * 'description' - The description of the image.
-- * 'errorCode' - The error code that is returned for the image.
-- * 'errorMessage' - The text of the error message that is returned for the image.
-- * 'imageId' - The identifier of the image.
-- * 'name' - The name of the image.
-- * 'operatingSystem' - The operating system that the image is running.
-- * 'ownerAccountId' - The identifier of the AWS account that owns the image.
-- * 'requiredTenancy' - Specifies whether the image is running on dedicated hardware. When Bring Your Own License (BYOL) is enabled, this value is set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
-- * 'state' - The status of the image.
mkWorkspaceImage ::
  WorkspaceImage
mkWorkspaceImage =
  WorkspaceImage'
    { state = Lude.Nothing,
      ownerAccountId = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      created = Lude.Nothing,
      requiredTenancy = Lude.Nothing,
      name = Lude.Nothing,
      imageId = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The status of the image.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiState :: Lens.Lens' WorkspaceImage (Lude.Maybe WorkspaceImageState)
wiState = Lens.lens (state :: WorkspaceImage -> Lude.Maybe WorkspaceImageState) (\s a -> s {state = a} :: WorkspaceImage)
{-# DEPRECATED wiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The identifier of the AWS account that owns the image.
--
-- /Note:/ Consider using 'ownerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiOwnerAccountId :: Lens.Lens' WorkspaceImage (Lude.Maybe Lude.Text)
wiOwnerAccountId = Lens.lens (ownerAccountId :: WorkspaceImage -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccountId = a} :: WorkspaceImage)
{-# DEPRECATED wiOwnerAccountId "Use generic-lens or generic-optics with 'ownerAccountId' instead." #-}

-- | The operating system that the image is running.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiOperatingSystem :: Lens.Lens' WorkspaceImage (Lude.Maybe OperatingSystem)
wiOperatingSystem = Lens.lens (operatingSystem :: WorkspaceImage -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: WorkspaceImage)
{-# DEPRECATED wiOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The date when the image was created. If the image has been shared, the AWS account that the image has been shared with sees the original creation date of the image.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiCreated :: Lens.Lens' WorkspaceImage (Lude.Maybe Lude.Timestamp)
wiCreated = Lens.lens (created :: WorkspaceImage -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: WorkspaceImage)
{-# DEPRECATED wiCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Specifies whether the image is running on dedicated hardware. When Bring Your Own License (BYOL) is enabled, this value is set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- /Note:/ Consider using 'requiredTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiRequiredTenancy :: Lens.Lens' WorkspaceImage (Lude.Maybe WorkspaceImageRequiredTenancy)
wiRequiredTenancy = Lens.lens (requiredTenancy :: WorkspaceImage -> Lude.Maybe WorkspaceImageRequiredTenancy) (\s a -> s {requiredTenancy = a} :: WorkspaceImage)
{-# DEPRECATED wiRequiredTenancy "Use generic-lens or generic-optics with 'requiredTenancy' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiName :: Lens.Lens' WorkspaceImage (Lude.Maybe Lude.Text)
wiName = Lens.lens (name :: WorkspaceImage -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: WorkspaceImage)
{-# DEPRECATED wiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiImageId :: Lens.Lens' WorkspaceImage (Lude.Maybe Lude.Text)
wiImageId = Lens.lens (imageId :: WorkspaceImage -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: WorkspaceImage)
{-# DEPRECATED wiImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The error code that is returned for the image.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiErrorCode :: Lens.Lens' WorkspaceImage (Lude.Maybe Lude.Text)
wiErrorCode = Lens.lens (errorCode :: WorkspaceImage -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: WorkspaceImage)
{-# DEPRECATED wiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The text of the error message that is returned for the image.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiErrorMessage :: Lens.Lens' WorkspaceImage (Lude.Maybe Lude.Text)
wiErrorMessage = Lens.lens (errorMessage :: WorkspaceImage -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: WorkspaceImage)
{-# DEPRECATED wiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wiDescription :: Lens.Lens' WorkspaceImage (Lude.Maybe Lude.Text)
wiDescription = Lens.lens (description :: WorkspaceImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: WorkspaceImage)
{-# DEPRECATED wiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON WorkspaceImage where
  parseJSON =
    Lude.withObject
      "WorkspaceImage"
      ( \x ->
          WorkspaceImage'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "OwnerAccountId")
            Lude.<*> (x Lude..:? "OperatingSystem")
            Lude.<*> (x Lude..:? "Created")
            Lude.<*> (x Lude..:? "RequiredTenancy")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ImageId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "Description")
      )
