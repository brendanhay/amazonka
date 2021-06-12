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
-- Module      : Network.AWS.SageMaker.Types.ImageVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ImageVersionStatus

-- | A version of a SageMaker @Image@. A version represents an existing
-- container image.
--
-- /See:/ 'newImageVersion' smart constructor.
data ImageVersion = ImageVersion'
  { -- | When a create or delete operation fails, the reason for the failure.
    failureReason :: Core.Maybe Core.Text,
    -- | When the version was created.
    creationTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the image the version is based on.
    imageArn :: Core.Text,
    -- | The ARN of the version.
    imageVersionArn :: Core.Text,
    -- | The status of the version.
    imageVersionStatus :: ImageVersionStatus,
    -- | When the version was last modified.
    lastModifiedTime :: Core.POSIX,
    -- | The version number.
    version :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'imageVersion_failureReason' - When a create or delete operation fails, the reason for the failure.
--
-- 'creationTime', 'imageVersion_creationTime' - When the version was created.
--
-- 'imageArn', 'imageVersion_imageArn' - The Amazon Resource Name (ARN) of the image the version is based on.
--
-- 'imageVersionArn', 'imageVersion_imageVersionArn' - The ARN of the version.
--
-- 'imageVersionStatus', 'imageVersion_imageVersionStatus' - The status of the version.
--
-- 'lastModifiedTime', 'imageVersion_lastModifiedTime' - When the version was last modified.
--
-- 'version', 'imageVersion_version' - The version number.
newImageVersion ::
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'imageArn'
  Core.Text ->
  -- | 'imageVersionArn'
  Core.Text ->
  -- | 'imageVersionStatus'
  ImageVersionStatus ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'version'
  Core.Natural ->
  ImageVersion
newImageVersion
  pCreationTime_
  pImageArn_
  pImageVersionArn_
  pImageVersionStatus_
  pLastModifiedTime_
  pVersion_ =
    ImageVersion'
      { failureReason = Core.Nothing,
        creationTime = Core._Time Lens.# pCreationTime_,
        imageArn = pImageArn_,
        imageVersionArn = pImageVersionArn_,
        imageVersionStatus = pImageVersionStatus_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        version = pVersion_
      }

-- | When a create or delete operation fails, the reason for the failure.
imageVersion_failureReason :: Lens.Lens' ImageVersion (Core.Maybe Core.Text)
imageVersion_failureReason = Lens.lens (\ImageVersion' {failureReason} -> failureReason) (\s@ImageVersion' {} a -> s {failureReason = a} :: ImageVersion)

-- | When the version was created.
imageVersion_creationTime :: Lens.Lens' ImageVersion Core.UTCTime
imageVersion_creationTime = Lens.lens (\ImageVersion' {creationTime} -> creationTime) (\s@ImageVersion' {} a -> s {creationTime = a} :: ImageVersion) Core.. Core._Time

-- | The Amazon Resource Name (ARN) of the image the version is based on.
imageVersion_imageArn :: Lens.Lens' ImageVersion Core.Text
imageVersion_imageArn = Lens.lens (\ImageVersion' {imageArn} -> imageArn) (\s@ImageVersion' {} a -> s {imageArn = a} :: ImageVersion)

-- | The ARN of the version.
imageVersion_imageVersionArn :: Lens.Lens' ImageVersion Core.Text
imageVersion_imageVersionArn = Lens.lens (\ImageVersion' {imageVersionArn} -> imageVersionArn) (\s@ImageVersion' {} a -> s {imageVersionArn = a} :: ImageVersion)

-- | The status of the version.
imageVersion_imageVersionStatus :: Lens.Lens' ImageVersion ImageVersionStatus
imageVersion_imageVersionStatus = Lens.lens (\ImageVersion' {imageVersionStatus} -> imageVersionStatus) (\s@ImageVersion' {} a -> s {imageVersionStatus = a} :: ImageVersion)

-- | When the version was last modified.
imageVersion_lastModifiedTime :: Lens.Lens' ImageVersion Core.UTCTime
imageVersion_lastModifiedTime = Lens.lens (\ImageVersion' {lastModifiedTime} -> lastModifiedTime) (\s@ImageVersion' {} a -> s {lastModifiedTime = a} :: ImageVersion) Core.. Core._Time

-- | The version number.
imageVersion_version :: Lens.Lens' ImageVersion Core.Natural
imageVersion_version = Lens.lens (\ImageVersion' {version} -> version) (\s@ImageVersion' {} a -> s {version = a} :: ImageVersion)

instance Core.FromJSON ImageVersion where
  parseJSON =
    Core.withObject
      "ImageVersion"
      ( \x ->
          ImageVersion'
            Core.<$> (x Core..:? "FailureReason")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "ImageArn")
            Core.<*> (x Core..: "ImageVersionArn")
            Core.<*> (x Core..: "ImageVersionStatus")
            Core.<*> (x Core..: "LastModifiedTime")
            Core.<*> (x Core..: "Version")
      )

instance Core.Hashable ImageVersion

instance Core.NFData ImageVersion
