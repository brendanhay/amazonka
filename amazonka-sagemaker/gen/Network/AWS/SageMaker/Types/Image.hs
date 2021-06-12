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
import Network.AWS.SageMaker.Types.ImageStatus

-- | A SageMaker image. A SageMaker image represents a set of container
-- images that are derived from a common base container image. Each of
-- these container images is represented by a SageMaker @ImageVersion@.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | When a create, update, or delete operation fails, the reason for the
    -- failure.
    failureReason :: Core.Maybe Core.Text,
    -- | The description of the image.
    description :: Core.Maybe Core.Text,
    -- | The name of the image as displayed.
    displayName :: Core.Maybe Core.Text,
    -- | When the image was created.
    creationTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Core.Text,
    -- | The name of the image.
    imageName :: Core.Text,
    -- | The status of the image.
    imageStatus :: ImageStatus,
    -- | When the image was last modified.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'description', 'image_description' - The description of the image.
--
-- 'displayName', 'image_displayName' - The name of the image as displayed.
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
  Core.UTCTime ->
  -- | 'imageArn'
  Core.Text ->
  -- | 'imageName'
  Core.Text ->
  -- | 'imageStatus'
  ImageStatus ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  Image
newImage
  pCreationTime_
  pImageArn_
  pImageName_
  pImageStatus_
  pLastModifiedTime_ =
    Image'
      { failureReason = Core.Nothing,
        description = Core.Nothing,
        displayName = Core.Nothing,
        creationTime = Core._Time Lens.# pCreationTime_,
        imageArn = pImageArn_,
        imageName = pImageName_,
        imageStatus = pImageStatus_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | When a create, update, or delete operation fails, the reason for the
-- failure.
image_failureReason :: Lens.Lens' Image (Core.Maybe Core.Text)
image_failureReason = Lens.lens (\Image' {failureReason} -> failureReason) (\s@Image' {} a -> s {failureReason = a} :: Image)

-- | The description of the image.
image_description :: Lens.Lens' Image (Core.Maybe Core.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | The name of the image as displayed.
image_displayName :: Lens.Lens' Image (Core.Maybe Core.Text)
image_displayName = Lens.lens (\Image' {displayName} -> displayName) (\s@Image' {} a -> s {displayName = a} :: Image)

-- | When the image was created.
image_creationTime :: Lens.Lens' Image Core.UTCTime
image_creationTime = Lens.lens (\Image' {creationTime} -> creationTime) (\s@Image' {} a -> s {creationTime = a} :: Image) Core.. Core._Time

-- | The Amazon Resource Name (ARN) of the image.
image_imageArn :: Lens.Lens' Image Core.Text
image_imageArn = Lens.lens (\Image' {imageArn} -> imageArn) (\s@Image' {} a -> s {imageArn = a} :: Image)

-- | The name of the image.
image_imageName :: Lens.Lens' Image Core.Text
image_imageName = Lens.lens (\Image' {imageName} -> imageName) (\s@Image' {} a -> s {imageName = a} :: Image)

-- | The status of the image.
image_imageStatus :: Lens.Lens' Image ImageStatus
image_imageStatus = Lens.lens (\Image' {imageStatus} -> imageStatus) (\s@Image' {} a -> s {imageStatus = a} :: Image)

-- | When the image was last modified.
image_lastModifiedTime :: Lens.Lens' Image Core.UTCTime
image_lastModifiedTime = Lens.lens (\Image' {lastModifiedTime} -> lastModifiedTime) (\s@Image' {} a -> s {lastModifiedTime = a} :: Image) Core.. Core._Time

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject
      "Image"
      ( \x ->
          Image'
            Core.<$> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "ImageArn")
            Core.<*> (x Core..: "ImageName")
            Core.<*> (x Core..: "ImageStatus")
            Core.<*> (x Core..: "LastModifiedTime")
      )

instance Core.Hashable Image

instance Core.NFData Image
