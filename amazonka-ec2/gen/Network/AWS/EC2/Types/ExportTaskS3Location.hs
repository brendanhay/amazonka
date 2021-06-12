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
-- Module      : Network.AWS.EC2.Types.ExportTaskS3Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskS3Location where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the destination for an export image task.
--
-- /See:/ 'newExportTaskS3Location' smart constructor.
data ExportTaskS3Location = ExportTaskS3Location'
  { -- | The destination Amazon S3 bucket.
    s3Bucket :: Core.Maybe Core.Text,
    -- | The prefix (logical hierarchy) in the bucket.
    s3Prefix :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportTaskS3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'exportTaskS3Location_s3Bucket' - The destination Amazon S3 bucket.
--
-- 's3Prefix', 'exportTaskS3Location_s3Prefix' - The prefix (logical hierarchy) in the bucket.
newExportTaskS3Location ::
  ExportTaskS3Location
newExportTaskS3Location =
  ExportTaskS3Location'
    { s3Bucket = Core.Nothing,
      s3Prefix = Core.Nothing
    }

-- | The destination Amazon S3 bucket.
exportTaskS3Location_s3Bucket :: Lens.Lens' ExportTaskS3Location (Core.Maybe Core.Text)
exportTaskS3Location_s3Bucket = Lens.lens (\ExportTaskS3Location' {s3Bucket} -> s3Bucket) (\s@ExportTaskS3Location' {} a -> s {s3Bucket = a} :: ExportTaskS3Location)

-- | The prefix (logical hierarchy) in the bucket.
exportTaskS3Location_s3Prefix :: Lens.Lens' ExportTaskS3Location (Core.Maybe Core.Text)
exportTaskS3Location_s3Prefix = Lens.lens (\ExportTaskS3Location' {s3Prefix} -> s3Prefix) (\s@ExportTaskS3Location' {} a -> s {s3Prefix = a} :: ExportTaskS3Location)

instance Core.FromXML ExportTaskS3Location where
  parseXML x =
    ExportTaskS3Location'
      Core.<$> (x Core..@? "s3Bucket")
      Core.<*> (x Core..@? "s3Prefix")

instance Core.Hashable ExportTaskS3Location

instance Core.NFData ExportTaskS3Location
