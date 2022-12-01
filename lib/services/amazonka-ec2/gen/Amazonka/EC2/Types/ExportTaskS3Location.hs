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
-- Module      : Amazonka.EC2.Types.ExportTaskS3Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ExportTaskS3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination for an export image task.
--
-- /See:/ 'newExportTaskS3Location' smart constructor.
data ExportTaskS3Location = ExportTaskS3Location'
  { -- | The destination Amazon S3 bucket.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The prefix (logical hierarchy) in the bucket.
    s3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { s3Bucket = Prelude.Nothing,
      s3Prefix = Prelude.Nothing
    }

-- | The destination Amazon S3 bucket.
exportTaskS3Location_s3Bucket :: Lens.Lens' ExportTaskS3Location (Prelude.Maybe Prelude.Text)
exportTaskS3Location_s3Bucket = Lens.lens (\ExportTaskS3Location' {s3Bucket} -> s3Bucket) (\s@ExportTaskS3Location' {} a -> s {s3Bucket = a} :: ExportTaskS3Location)

-- | The prefix (logical hierarchy) in the bucket.
exportTaskS3Location_s3Prefix :: Lens.Lens' ExportTaskS3Location (Prelude.Maybe Prelude.Text)
exportTaskS3Location_s3Prefix = Lens.lens (\ExportTaskS3Location' {s3Prefix} -> s3Prefix) (\s@ExportTaskS3Location' {} a -> s {s3Prefix = a} :: ExportTaskS3Location)

instance Core.FromXML ExportTaskS3Location where
  parseXML x =
    ExportTaskS3Location'
      Prelude.<$> (x Core..@? "s3Bucket")
      Prelude.<*> (x Core..@? "s3Prefix")

instance Prelude.Hashable ExportTaskS3Location where
  hashWithSalt _salt ExportTaskS3Location' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Prefix

instance Prelude.NFData ExportTaskS3Location where
  rnf ExportTaskS3Location' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Prefix
