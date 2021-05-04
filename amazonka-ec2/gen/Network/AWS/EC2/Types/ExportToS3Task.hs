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
-- Module      : Network.AWS.EC2.Types.ExportToS3Task
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportToS3Task where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ContainerFormat
import Network.AWS.EC2.Types.DiskImageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the format and location for the export task.
--
-- /See:/ 'newExportToS3Task' smart constructor.
data ExportToS3Task = ExportToS3Task'
  { -- | The container format used to combine disk images with metadata (such as
    -- OVF). If absent, only the disk image is exported.
    containerFormat :: Prelude.Maybe ContainerFormat,
    -- | The format for the exported image.
    diskImageFormat :: Prelude.Maybe DiskImageFormat,
    -- | The Amazon S3 bucket for the destination image. The destination bucket
    -- must exist and grant WRITE and READ_ACP permissions to the AWS account
    -- @vm-import-export\@amazon.com@.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The encryption key for your S3 bucket.
    s3Key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportToS3Task' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerFormat', 'exportToS3Task_containerFormat' - The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
--
-- 'diskImageFormat', 'exportToS3Task_diskImageFormat' - The format for the exported image.
--
-- 's3Bucket', 'exportToS3Task_s3Bucket' - The Amazon S3 bucket for the destination image. The destination bucket
-- must exist and grant WRITE and READ_ACP permissions to the AWS account
-- @vm-import-export\@amazon.com@.
--
-- 's3Key', 'exportToS3Task_s3Key' - The encryption key for your S3 bucket.
newExportToS3Task ::
  ExportToS3Task
newExportToS3Task =
  ExportToS3Task'
    { containerFormat = Prelude.Nothing,
      diskImageFormat = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing
    }

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
exportToS3Task_containerFormat :: Lens.Lens' ExportToS3Task (Prelude.Maybe ContainerFormat)
exportToS3Task_containerFormat = Lens.lens (\ExportToS3Task' {containerFormat} -> containerFormat) (\s@ExportToS3Task' {} a -> s {containerFormat = a} :: ExportToS3Task)

-- | The format for the exported image.
exportToS3Task_diskImageFormat :: Lens.Lens' ExportToS3Task (Prelude.Maybe DiskImageFormat)
exportToS3Task_diskImageFormat = Lens.lens (\ExportToS3Task' {diskImageFormat} -> diskImageFormat) (\s@ExportToS3Task' {} a -> s {diskImageFormat = a} :: ExportToS3Task)

-- | The Amazon S3 bucket for the destination image. The destination bucket
-- must exist and grant WRITE and READ_ACP permissions to the AWS account
-- @vm-import-export\@amazon.com@.
exportToS3Task_s3Bucket :: Lens.Lens' ExportToS3Task (Prelude.Maybe Prelude.Text)
exportToS3Task_s3Bucket = Lens.lens (\ExportToS3Task' {s3Bucket} -> s3Bucket) (\s@ExportToS3Task' {} a -> s {s3Bucket = a} :: ExportToS3Task)

-- | The encryption key for your S3 bucket.
exportToS3Task_s3Key :: Lens.Lens' ExportToS3Task (Prelude.Maybe Prelude.Text)
exportToS3Task_s3Key = Lens.lens (\ExportToS3Task' {s3Key} -> s3Key) (\s@ExportToS3Task' {} a -> s {s3Key = a} :: ExportToS3Task)

instance Prelude.FromXML ExportToS3Task where
  parseXML x =
    ExportToS3Task'
      Prelude.<$> (x Prelude..@? "containerFormat")
      Prelude.<*> (x Prelude..@? "diskImageFormat")
      Prelude.<*> (x Prelude..@? "s3Bucket")
      Prelude.<*> (x Prelude..@? "s3Key")

instance Prelude.Hashable ExportToS3Task

instance Prelude.NFData ExportToS3Task
