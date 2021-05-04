{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceImage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.OperatingSystem
import Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy
import Network.AWS.WorkSpaces.Types.WorkspaceImageState

-- | Describes a WorkSpace image.
--
-- /See:/ 'newWorkspaceImage' smart constructor.
data WorkspaceImage = WorkspaceImage'
  { -- | The identifier of the image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The status of the image.
    state :: Prelude.Maybe WorkspaceImageState,
    -- | The name of the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message that is returned for the image.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the image is running on dedicated hardware. When Bring
    -- Your Own License (BYOL) is enabled, this value is set to @DEDICATED@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
    requiredTenancy :: Prelude.Maybe WorkspaceImageRequiredTenancy,
    -- | The operating system that the image is running.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | The date when the image was created. If the image has been shared, the
    -- AWS account that the image has been shared with sees the original
    -- creation date of the image.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the AWS account that owns the image.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The error code that is returned for the image.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'workspaceImage_imageId' - The identifier of the image.
--
-- 'state', 'workspaceImage_state' - The status of the image.
--
-- 'name', 'workspaceImage_name' - The name of the image.
--
-- 'description', 'workspaceImage_description' - The description of the image.
--
-- 'errorMessage', 'workspaceImage_errorMessage' - The text of the error message that is returned for the image.
--
-- 'requiredTenancy', 'workspaceImage_requiredTenancy' - Specifies whether the image is running on dedicated hardware. When Bring
-- Your Own License (BYOL) is enabled, this value is set to @DEDICATED@.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
--
-- 'operatingSystem', 'workspaceImage_operatingSystem' - The operating system that the image is running.
--
-- 'created', 'workspaceImage_created' - The date when the image was created. If the image has been shared, the
-- AWS account that the image has been shared with sees the original
-- creation date of the image.
--
-- 'ownerAccountId', 'workspaceImage_ownerAccountId' - The identifier of the AWS account that owns the image.
--
-- 'errorCode', 'workspaceImage_errorCode' - The error code that is returned for the image.
newWorkspaceImage ::
  WorkspaceImage
newWorkspaceImage =
  WorkspaceImage'
    { imageId = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      requiredTenancy = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      created = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The identifier of the image.
workspaceImage_imageId :: Lens.Lens' WorkspaceImage (Prelude.Maybe Prelude.Text)
workspaceImage_imageId = Lens.lens (\WorkspaceImage' {imageId} -> imageId) (\s@WorkspaceImage' {} a -> s {imageId = a} :: WorkspaceImage)

-- | The status of the image.
workspaceImage_state :: Lens.Lens' WorkspaceImage (Prelude.Maybe WorkspaceImageState)
workspaceImage_state = Lens.lens (\WorkspaceImage' {state} -> state) (\s@WorkspaceImage' {} a -> s {state = a} :: WorkspaceImage)

-- | The name of the image.
workspaceImage_name :: Lens.Lens' WorkspaceImage (Prelude.Maybe Prelude.Text)
workspaceImage_name = Lens.lens (\WorkspaceImage' {name} -> name) (\s@WorkspaceImage' {} a -> s {name = a} :: WorkspaceImage)

-- | The description of the image.
workspaceImage_description :: Lens.Lens' WorkspaceImage (Prelude.Maybe Prelude.Text)
workspaceImage_description = Lens.lens (\WorkspaceImage' {description} -> description) (\s@WorkspaceImage' {} a -> s {description = a} :: WorkspaceImage)

-- | The text of the error message that is returned for the image.
workspaceImage_errorMessage :: Lens.Lens' WorkspaceImage (Prelude.Maybe Prelude.Text)
workspaceImage_errorMessage = Lens.lens (\WorkspaceImage' {errorMessage} -> errorMessage) (\s@WorkspaceImage' {} a -> s {errorMessage = a} :: WorkspaceImage)

-- | Specifies whether the image is running on dedicated hardware. When Bring
-- Your Own License (BYOL) is enabled, this value is set to @DEDICATED@.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
workspaceImage_requiredTenancy :: Lens.Lens' WorkspaceImage (Prelude.Maybe WorkspaceImageRequiredTenancy)
workspaceImage_requiredTenancy = Lens.lens (\WorkspaceImage' {requiredTenancy} -> requiredTenancy) (\s@WorkspaceImage' {} a -> s {requiredTenancy = a} :: WorkspaceImage)

-- | The operating system that the image is running.
workspaceImage_operatingSystem :: Lens.Lens' WorkspaceImage (Prelude.Maybe OperatingSystem)
workspaceImage_operatingSystem = Lens.lens (\WorkspaceImage' {operatingSystem} -> operatingSystem) (\s@WorkspaceImage' {} a -> s {operatingSystem = a} :: WorkspaceImage)

-- | The date when the image was created. If the image has been shared, the
-- AWS account that the image has been shared with sees the original
-- creation date of the image.
workspaceImage_created :: Lens.Lens' WorkspaceImage (Prelude.Maybe Prelude.UTCTime)
workspaceImage_created = Lens.lens (\WorkspaceImage' {created} -> created) (\s@WorkspaceImage' {} a -> s {created = a} :: WorkspaceImage) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the AWS account that owns the image.
workspaceImage_ownerAccountId :: Lens.Lens' WorkspaceImage (Prelude.Maybe Prelude.Text)
workspaceImage_ownerAccountId = Lens.lens (\WorkspaceImage' {ownerAccountId} -> ownerAccountId) (\s@WorkspaceImage' {} a -> s {ownerAccountId = a} :: WorkspaceImage)

-- | The error code that is returned for the image.
workspaceImage_errorCode :: Lens.Lens' WorkspaceImage (Prelude.Maybe Prelude.Text)
workspaceImage_errorCode = Lens.lens (\WorkspaceImage' {errorCode} -> errorCode) (\s@WorkspaceImage' {} a -> s {errorCode = a} :: WorkspaceImage)

instance Prelude.FromJSON WorkspaceImage where
  parseJSON =
    Prelude.withObject
      "WorkspaceImage"
      ( \x ->
          WorkspaceImage'
            Prelude.<$> (x Prelude..:? "ImageId")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "RequiredTenancy")
            Prelude.<*> (x Prelude..:? "OperatingSystem")
            Prelude.<*> (x Prelude..:? "Created")
            Prelude.<*> (x Prelude..:? "OwnerAccountId")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable WorkspaceImage

instance Prelude.NFData WorkspaceImage
