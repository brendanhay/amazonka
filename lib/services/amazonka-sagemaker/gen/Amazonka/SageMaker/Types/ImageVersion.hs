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
-- Module      : Amazonka.SageMaker.Types.ImageVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ImageVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ImageVersionStatus

-- | A version of a SageMaker @Image@. A version represents an existing
-- container image.
--
-- /See:/ 'newImageVersion' smart constructor.
data ImageVersion = ImageVersion'
  { -- | When a create or delete operation fails, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | When the version was created.
    creationTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the image the version is based on.
    imageArn :: Prelude.Text,
    -- | The ARN of the version.
    imageVersionArn :: Prelude.Text,
    -- | The status of the version.
    imageVersionStatus :: ImageVersionStatus,
    -- | When the version was last modified.
    lastModifiedTime :: Core.POSIX,
    -- | The version number.
    version :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.UTCTime ->
  -- | 'imageArn'
  Prelude.Text ->
  -- | 'imageVersionArn'
  Prelude.Text ->
  -- | 'imageVersionStatus'
  ImageVersionStatus ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'version'
  Prelude.Natural ->
  ImageVersion
newImageVersion
  pCreationTime_
  pImageArn_
  pImageVersionArn_
  pImageVersionStatus_
  pLastModifiedTime_
  pVersion_ =
    ImageVersion'
      { failureReason = Prelude.Nothing,
        creationTime = Core._Time Lens.# pCreationTime_,
        imageArn = pImageArn_,
        imageVersionArn = pImageVersionArn_,
        imageVersionStatus = pImageVersionStatus_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        version = pVersion_
      }

-- | When a create or delete operation fails, the reason for the failure.
imageVersion_failureReason :: Lens.Lens' ImageVersion (Prelude.Maybe Prelude.Text)
imageVersion_failureReason = Lens.lens (\ImageVersion' {failureReason} -> failureReason) (\s@ImageVersion' {} a -> s {failureReason = a} :: ImageVersion)

-- | When the version was created.
imageVersion_creationTime :: Lens.Lens' ImageVersion Prelude.UTCTime
imageVersion_creationTime = Lens.lens (\ImageVersion' {creationTime} -> creationTime) (\s@ImageVersion' {} a -> s {creationTime = a} :: ImageVersion) Prelude.. Core._Time

-- | The Amazon Resource Name (ARN) of the image the version is based on.
imageVersion_imageArn :: Lens.Lens' ImageVersion Prelude.Text
imageVersion_imageArn = Lens.lens (\ImageVersion' {imageArn} -> imageArn) (\s@ImageVersion' {} a -> s {imageArn = a} :: ImageVersion)

-- | The ARN of the version.
imageVersion_imageVersionArn :: Lens.Lens' ImageVersion Prelude.Text
imageVersion_imageVersionArn = Lens.lens (\ImageVersion' {imageVersionArn} -> imageVersionArn) (\s@ImageVersion' {} a -> s {imageVersionArn = a} :: ImageVersion)

-- | The status of the version.
imageVersion_imageVersionStatus :: Lens.Lens' ImageVersion ImageVersionStatus
imageVersion_imageVersionStatus = Lens.lens (\ImageVersion' {imageVersionStatus} -> imageVersionStatus) (\s@ImageVersion' {} a -> s {imageVersionStatus = a} :: ImageVersion)

-- | When the version was last modified.
imageVersion_lastModifiedTime :: Lens.Lens' ImageVersion Prelude.UTCTime
imageVersion_lastModifiedTime = Lens.lens (\ImageVersion' {lastModifiedTime} -> lastModifiedTime) (\s@ImageVersion' {} a -> s {lastModifiedTime = a} :: ImageVersion) Prelude.. Core._Time

-- | The version number.
imageVersion_version :: Lens.Lens' ImageVersion Prelude.Natural
imageVersion_version = Lens.lens (\ImageVersion' {version} -> version) (\s@ImageVersion' {} a -> s {version = a} :: ImageVersion)

instance Core.FromJSON ImageVersion where
  parseJSON =
    Core.withObject
      "ImageVersion"
      ( \x ->
          ImageVersion'
            Prelude.<$> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "ImageArn")
            Prelude.<*> (x Core..: "ImageVersionArn")
            Prelude.<*> (x Core..: "ImageVersionStatus")
            Prelude.<*> (x Core..: "LastModifiedTime")
            Prelude.<*> (x Core..: "Version")
      )

instance Prelude.Hashable ImageVersion where
  hashWithSalt _salt ImageVersion' {..} =
    _salt `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` imageArn
      `Prelude.hashWithSalt` imageVersionArn
      `Prelude.hashWithSalt` imageVersionStatus
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` version

instance Prelude.NFData ImageVersion where
  rnf ImageVersion' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf imageArn
      `Prelude.seq` Prelude.rnf imageVersionArn
      `Prelude.seq` Prelude.rnf imageVersionStatus
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf version
