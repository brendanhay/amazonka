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
-- Module      : Amazonka.SageMaker.Types.Image
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Image where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ImageStatus

-- | A SageMaker image. A SageMaker image represents a set of container
-- images that are derived from a common base container image. Each of
-- these container images is represented by a SageMaker @ImageVersion@.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the image as displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | When a create, update, or delete operation fails, the reason for the
    -- failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | When the image was created.
    creationTime :: Data.POSIX,
    -- | The ARN of the image.
    imageArn :: Prelude.Text,
    -- | The name of the image.
    imageName :: Prelude.Text,
    -- | The status of the image.
    imageStatus :: ImageStatus,
    -- | When the image was last modified.
    lastModifiedTime :: Data.POSIX
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
-- 'description', 'image_description' - The description of the image.
--
-- 'displayName', 'image_displayName' - The name of the image as displayed.
--
-- 'failureReason', 'image_failureReason' - When a create, update, or delete operation fails, the reason for the
-- failure.
--
-- 'creationTime', 'image_creationTime' - When the image was created.
--
-- 'imageArn', 'image_imageArn' - The ARN of the image.
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
      { description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        creationTime = Data._Time Lens.# pCreationTime_,
        imageArn = pImageArn_,
        imageName = pImageName_,
        imageStatus = pImageStatus_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | The description of the image.
image_description :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | The name of the image as displayed.
image_displayName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_displayName = Lens.lens (\Image' {displayName} -> displayName) (\s@Image' {} a -> s {displayName = a} :: Image)

-- | When a create, update, or delete operation fails, the reason for the
-- failure.
image_failureReason :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_failureReason = Lens.lens (\Image' {failureReason} -> failureReason) (\s@Image' {} a -> s {failureReason = a} :: Image)

-- | When the image was created.
image_creationTime :: Lens.Lens' Image Prelude.UTCTime
image_creationTime = Lens.lens (\Image' {creationTime} -> creationTime) (\s@Image' {} a -> s {creationTime = a} :: Image) Prelude.. Data._Time

-- | The ARN of the image.
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
image_lastModifiedTime = Lens.lens (\Image' {lastModifiedTime} -> lastModifiedTime) (\s@Image' {} a -> s {lastModifiedTime = a} :: Image) Prelude.. Data._Time

instance Data.FromJSON Image where
  parseJSON =
    Data.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "ImageArn")
            Prelude.<*> (x Data..: "ImageName")
            Prelude.<*> (x Data..: "ImageStatus")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable Image where
  hashWithSalt _salt Image' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` imageArn
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` imageStatus
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf imageArn
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf imageStatus
      `Prelude.seq` Prelude.rnf lastModifiedTime
