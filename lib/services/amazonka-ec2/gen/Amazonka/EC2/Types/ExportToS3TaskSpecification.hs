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
-- Module      : Amazonka.EC2.Types.ExportToS3TaskSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ExportToS3TaskSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ContainerFormat
import Amazonka.EC2.Types.DiskImageFormat
import qualified Amazonka.Prelude as Prelude

-- | Describes an export instance task.
--
-- /See:/ 'newExportToS3TaskSpecification' smart constructor.
data ExportToS3TaskSpecification = ExportToS3TaskSpecification'
  { -- | The Amazon S3 bucket for the destination image. The destination bucket
    -- must exist and have an access control list (ACL) attached that specifies
    -- the Region-specific canonical account ID for the @Grantee@. For more
    -- information about the ACL to your S3 bucket, see
    -- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html#vmexport-prerequisites Prerequisites>
    -- in the VM Import\/Export User Guide.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The format for the exported image.
    diskImageFormat :: Prelude.Maybe DiskImageFormat,
    -- | The container format used to combine disk images with metadata (such as
    -- OVF). If absent, only the disk image is exported.
    containerFormat :: Prelude.Maybe ContainerFormat,
    -- | The image is written to a single object in the Amazon S3 bucket at the
    -- S3 key s3prefix + exportTaskId + \'.\' + diskImageFormat.
    s3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportToS3TaskSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'exportToS3TaskSpecification_s3Bucket' - The Amazon S3 bucket for the destination image. The destination bucket
-- must exist and have an access control list (ACL) attached that specifies
-- the Region-specific canonical account ID for the @Grantee@. For more
-- information about the ACL to your S3 bucket, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html#vmexport-prerequisites Prerequisites>
-- in the VM Import\/Export User Guide.
--
-- 'diskImageFormat', 'exportToS3TaskSpecification_diskImageFormat' - The format for the exported image.
--
-- 'containerFormat', 'exportToS3TaskSpecification_containerFormat' - The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
--
-- 's3Prefix', 'exportToS3TaskSpecification_s3Prefix' - The image is written to a single object in the Amazon S3 bucket at the
-- S3 key s3prefix + exportTaskId + \'.\' + diskImageFormat.
newExportToS3TaskSpecification ::
  ExportToS3TaskSpecification
newExportToS3TaskSpecification =
  ExportToS3TaskSpecification'
    { s3Bucket =
        Prelude.Nothing,
      diskImageFormat = Prelude.Nothing,
      containerFormat = Prelude.Nothing,
      s3Prefix = Prelude.Nothing
    }

-- | The Amazon S3 bucket for the destination image. The destination bucket
-- must exist and have an access control list (ACL) attached that specifies
-- the Region-specific canonical account ID for the @Grantee@. For more
-- information about the ACL to your S3 bucket, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html#vmexport-prerequisites Prerequisites>
-- in the VM Import\/Export User Guide.
exportToS3TaskSpecification_s3Bucket :: Lens.Lens' ExportToS3TaskSpecification (Prelude.Maybe Prelude.Text)
exportToS3TaskSpecification_s3Bucket = Lens.lens (\ExportToS3TaskSpecification' {s3Bucket} -> s3Bucket) (\s@ExportToS3TaskSpecification' {} a -> s {s3Bucket = a} :: ExportToS3TaskSpecification)

-- | The format for the exported image.
exportToS3TaskSpecification_diskImageFormat :: Lens.Lens' ExportToS3TaskSpecification (Prelude.Maybe DiskImageFormat)
exportToS3TaskSpecification_diskImageFormat = Lens.lens (\ExportToS3TaskSpecification' {diskImageFormat} -> diskImageFormat) (\s@ExportToS3TaskSpecification' {} a -> s {diskImageFormat = a} :: ExportToS3TaskSpecification)

-- | The container format used to combine disk images with metadata (such as
-- OVF). If absent, only the disk image is exported.
exportToS3TaskSpecification_containerFormat :: Lens.Lens' ExportToS3TaskSpecification (Prelude.Maybe ContainerFormat)
exportToS3TaskSpecification_containerFormat = Lens.lens (\ExportToS3TaskSpecification' {containerFormat} -> containerFormat) (\s@ExportToS3TaskSpecification' {} a -> s {containerFormat = a} :: ExportToS3TaskSpecification)

-- | The image is written to a single object in the Amazon S3 bucket at the
-- S3 key s3prefix + exportTaskId + \'.\' + diskImageFormat.
exportToS3TaskSpecification_s3Prefix :: Lens.Lens' ExportToS3TaskSpecification (Prelude.Maybe Prelude.Text)
exportToS3TaskSpecification_s3Prefix = Lens.lens (\ExportToS3TaskSpecification' {s3Prefix} -> s3Prefix) (\s@ExportToS3TaskSpecification' {} a -> s {s3Prefix = a} :: ExportToS3TaskSpecification)

instance Prelude.Hashable ExportToS3TaskSpecification where
  hashWithSalt _salt ExportToS3TaskSpecification' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` diskImageFormat
      `Prelude.hashWithSalt` containerFormat
      `Prelude.hashWithSalt` s3Prefix

instance Prelude.NFData ExportToS3TaskSpecification where
  rnf ExportToS3TaskSpecification' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf diskImageFormat
      `Prelude.seq` Prelude.rnf containerFormat
      `Prelude.seq` Prelude.rnf s3Prefix

instance Core.ToQuery ExportToS3TaskSpecification where
  toQuery ExportToS3TaskSpecification' {..} =
    Prelude.mconcat
      [ "S3Bucket" Core.=: s3Bucket,
        "DiskImageFormat" Core.=: diskImageFormat,
        "ContainerFormat" Core.=: containerFormat,
        "S3Prefix" Core.=: s3Prefix
      ]
