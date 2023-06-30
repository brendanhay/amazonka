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
-- Module      : Amazonka.ImageBuilder.Types.S3ExportConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.S3ExportConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.DiskImageFormat
import qualified Amazonka.Prelude as Prelude

-- | Properties that configure export from your build instance to a
-- compatible file format for your VM.
--
-- /See:/ 'newS3ExportConfiguration' smart constructor.
data S3ExportConfiguration = S3ExportConfiguration'
  { -- | The Amazon S3 path for the bucket where the output disk images for your
    -- VM are stored.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the role that grants VM Import\/Export permission to export
    -- images to your S3 bucket.
    roleName :: Prelude.Text,
    -- | Export the updated image to one of the following supported disk image
    -- formats:
    --
    -- -   __Virtual Hard Disk (VHD)__ – Compatible with Citrix Xen and
    --     Microsoft Hyper-V virtualization products.
    --
    -- -   __Stream-optimized ESX Virtual Machine Disk (VMDK)__ – Compatible
    --     with VMware ESX and VMware vSphere versions 4, 5, and 6.
    --
    -- -   __Raw__ – Raw format.
    diskImageFormat :: DiskImageFormat,
    -- | The S3 bucket in which to store the output disk images for your VM.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ExportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Prefix', 's3ExportConfiguration_s3Prefix' - The Amazon S3 path for the bucket where the output disk images for your
-- VM are stored.
--
-- 'roleName', 's3ExportConfiguration_roleName' - The name of the role that grants VM Import\/Export permission to export
-- images to your S3 bucket.
--
-- 'diskImageFormat', 's3ExportConfiguration_diskImageFormat' - Export the updated image to one of the following supported disk image
-- formats:
--
-- -   __Virtual Hard Disk (VHD)__ – Compatible with Citrix Xen and
--     Microsoft Hyper-V virtualization products.
--
-- -   __Stream-optimized ESX Virtual Machine Disk (VMDK)__ – Compatible
--     with VMware ESX and VMware vSphere versions 4, 5, and 6.
--
-- -   __Raw__ – Raw format.
--
-- 's3Bucket', 's3ExportConfiguration_s3Bucket' - The S3 bucket in which to store the output disk images for your VM.
newS3ExportConfiguration ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'diskImageFormat'
  DiskImageFormat ->
  -- | 's3Bucket'
  Prelude.Text ->
  S3ExportConfiguration
newS3ExportConfiguration
  pRoleName_
  pDiskImageFormat_
  pS3Bucket_ =
    S3ExportConfiguration'
      { s3Prefix = Prelude.Nothing,
        roleName = pRoleName_,
        diskImageFormat = pDiskImageFormat_,
        s3Bucket = pS3Bucket_
      }

-- | The Amazon S3 path for the bucket where the output disk images for your
-- VM are stored.
s3ExportConfiguration_s3Prefix :: Lens.Lens' S3ExportConfiguration (Prelude.Maybe Prelude.Text)
s3ExportConfiguration_s3Prefix = Lens.lens (\S3ExportConfiguration' {s3Prefix} -> s3Prefix) (\s@S3ExportConfiguration' {} a -> s {s3Prefix = a} :: S3ExportConfiguration)

-- | The name of the role that grants VM Import\/Export permission to export
-- images to your S3 bucket.
s3ExportConfiguration_roleName :: Lens.Lens' S3ExportConfiguration Prelude.Text
s3ExportConfiguration_roleName = Lens.lens (\S3ExportConfiguration' {roleName} -> roleName) (\s@S3ExportConfiguration' {} a -> s {roleName = a} :: S3ExportConfiguration)

-- | Export the updated image to one of the following supported disk image
-- formats:
--
-- -   __Virtual Hard Disk (VHD)__ – Compatible with Citrix Xen and
--     Microsoft Hyper-V virtualization products.
--
-- -   __Stream-optimized ESX Virtual Machine Disk (VMDK)__ – Compatible
--     with VMware ESX and VMware vSphere versions 4, 5, and 6.
--
-- -   __Raw__ – Raw format.
s3ExportConfiguration_diskImageFormat :: Lens.Lens' S3ExportConfiguration DiskImageFormat
s3ExportConfiguration_diskImageFormat = Lens.lens (\S3ExportConfiguration' {diskImageFormat} -> diskImageFormat) (\s@S3ExportConfiguration' {} a -> s {diskImageFormat = a} :: S3ExportConfiguration)

-- | The S3 bucket in which to store the output disk images for your VM.
s3ExportConfiguration_s3Bucket :: Lens.Lens' S3ExportConfiguration Prelude.Text
s3ExportConfiguration_s3Bucket = Lens.lens (\S3ExportConfiguration' {s3Bucket} -> s3Bucket) (\s@S3ExportConfiguration' {} a -> s {s3Bucket = a} :: S3ExportConfiguration)

instance Data.FromJSON S3ExportConfiguration where
  parseJSON =
    Data.withObject
      "S3ExportConfiguration"
      ( \x ->
          S3ExportConfiguration'
            Prelude.<$> (x Data..:? "s3Prefix")
            Prelude.<*> (x Data..: "roleName")
            Prelude.<*> (x Data..: "diskImageFormat")
            Prelude.<*> (x Data..: "s3Bucket")
      )

instance Prelude.Hashable S3ExportConfiguration where
  hashWithSalt _salt S3ExportConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` diskImageFormat
      `Prelude.hashWithSalt` s3Bucket

instance Prelude.NFData S3ExportConfiguration where
  rnf S3ExportConfiguration' {..} =
    Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf diskImageFormat
      `Prelude.seq` Prelude.rnf s3Bucket

instance Data.ToJSON S3ExportConfiguration where
  toJSON S3ExportConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3Prefix" Data..=) Prelude.<$> s3Prefix,
            Prelude.Just ("roleName" Data..= roleName),
            Prelude.Just
              ("diskImageFormat" Data..= diskImageFormat),
            Prelude.Just ("s3Bucket" Data..= s3Bucket)
          ]
      )
