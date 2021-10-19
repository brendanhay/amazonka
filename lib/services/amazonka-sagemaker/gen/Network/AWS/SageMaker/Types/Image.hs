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
-- Module      : Network.AWS.SageMaker.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Image where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ImageStatus

-- | A SageMaker image. A SageMaker image represents a set of container
-- images that are derived from a common base container image. Each of
-- these container images is represented by a SageMaker @ImageVersion@.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | When a create, update, or delete operation fails, the reason for the
    -- failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the image as displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the image was created.
    creationTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Prelude.Text,
    -- | The name of the image.
    imageName :: Prelude.Text,
    -- | The status of the image.
    imageStatus :: ImageStatus,
    -- | When the image was last modified.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'image_failureReason' - When a create, update, or delete operation fails, the reason for the
-- failure.
--
-- 'displayName', 'image_displayName' - The name of the image as displayed.
--
-- 'description', 'image_description' - The description of the image.
--
-- 'creationTime', 'image_creationTime' - When the image was created.
--
-- 'imageArn', 'image_imageArn' - The Amazon Resource Name (ARN) of the image.
--
-- 'imageName', 'image_imageName' - The name of the image.
--
-- 'imageStatus', 'image_imageStatus' - The status of the image.
--
-- 'lastModifiedTime', 'image_lastModifiedTime' - When the image was last modified.
newImage ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'imageArn'
  Prelude.Text ->
  -- | 'imageName'
  Prelude.Text ->
  -- | 'imageStatus'
  ImageStatus ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  Image
newImage
  pCreationTime_
  pImageArn_
  pImageName_
  pImageStatus_
  pLastModifiedTime_ =
    Image'
      { failureReason = Prelude.Nothing,
        displayName = Prelude.Nothing,
        description = Prelude.Nothing,
        creationTime = Core._Time Lens.# pCreationTime_,
        imageArn = pImageArn_,
        imageName = pImageName_,
        imageStatus = pImageStatus_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | When a create, update, or delete operation fails, the reason for the
-- failure.
image_failureReason :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_failureReason = Lens.lens (\Image' {failureReason} -> failureReason) (\s@Image' {} a -> s {failureReason = a} :: Image)

-- | The name of the image as displayed.
image_displayName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_displayName = Lens.lens (\Image' {displayName} -> displayName) (\s@Image' {} a -> s {displayName = a} :: Image)

-- | The description of the image.
image_description :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | When the image was created.
image_creationTime :: Lens.Lens' Image Prelude.UTCTime
image_creationTime = Lens.lens (\Image' {creationTime} -> creationTime) (\s@Image' {} a -> s {creationTime = a} :: Image) Prelude.. Core._Time

-- | The Amazon Resource Name (ARN) of the image.
image_imageArn :: Lens.Lens' Image Prelude.Text
image_imageArn = Lens.lens (\Image' {imageArn} -> imageArn) (\s@Image' {} a -> s {imageArn = a} :: Image)

-- | The name of the image.
image_imageName :: Lens.Lens' Image Prelude.Text
image_imageName = Lens.lens (\Image' {imageName} -> imageName) (\s@Image' {} a -> s {imageName = a} :: Image)

-- | The status of the image.
image_imageStatus :: Lens.Lens' Image ImageStatus
image_imageStatus = Lens.lens (\Image' {imageStatus} -> imageStatus) (\s@Image' {} a -> s {imageStatus = a} :: Image)

-- | When the image was last modified.
image_lastModifiedTime :: Lens.Lens' Image Prelude.UTCTime
image_lastModifiedTime = Lens.lens (\Image' {lastModifiedTime} -> lastModifiedTime) (\s@Image' {} a -> s {lastModifiedTime = a} :: Image) Prelude.. Core._Time

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "ImageArn")
            Prelude.<*> (x Core..: "ImageName")
            Prelude.<*> (x Core..: "ImageStatus")
            Prelude.<*> (x Core..: "LastModifiedTime")
      )

instance Prelude.Hashable Image

instance Prelude.NFData Image
