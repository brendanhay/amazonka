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
-- Module      : Network.AWS.EC2.Types.ExportTaskS3LocationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTaskS3LocationRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the destination for an export image task.
--
-- /See:/ 'newExportTaskS3LocationRequest' smart constructor.
data ExportTaskS3LocationRequest = ExportTaskS3LocationRequest'
  { -- | The prefix (logical hierarchy) in the bucket.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The destination Amazon S3 bucket.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportTaskS3LocationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Prefix', 'exportTaskS3LocationRequest_s3Prefix' - The prefix (logical hierarchy) in the bucket.
--
-- 's3Bucket', 'exportTaskS3LocationRequest_s3Bucket' - The destination Amazon S3 bucket.
newExportTaskS3LocationRequest ::
  -- | 's3Bucket'
  Prelude.Text ->
  ExportTaskS3LocationRequest
newExportTaskS3LocationRequest pS3Bucket_ =
  ExportTaskS3LocationRequest'
    { s3Prefix =
        Prelude.Nothing,
      s3Bucket = pS3Bucket_
    }

-- | The prefix (logical hierarchy) in the bucket.
exportTaskS3LocationRequest_s3Prefix :: Lens.Lens' ExportTaskS3LocationRequest (Prelude.Maybe Prelude.Text)
exportTaskS3LocationRequest_s3Prefix = Lens.lens (\ExportTaskS3LocationRequest' {s3Prefix} -> s3Prefix) (\s@ExportTaskS3LocationRequest' {} a -> s {s3Prefix = a} :: ExportTaskS3LocationRequest)

-- | The destination Amazon S3 bucket.
exportTaskS3LocationRequest_s3Bucket :: Lens.Lens' ExportTaskS3LocationRequest Prelude.Text
exportTaskS3LocationRequest_s3Bucket = Lens.lens (\ExportTaskS3LocationRequest' {s3Bucket} -> s3Bucket) (\s@ExportTaskS3LocationRequest' {} a -> s {s3Bucket = a} :: ExportTaskS3LocationRequest)

instance Prelude.Hashable ExportTaskS3LocationRequest

instance Prelude.NFData ExportTaskS3LocationRequest

instance Prelude.ToQuery ExportTaskS3LocationRequest where
  toQuery ExportTaskS3LocationRequest' {..} =
    Prelude.mconcat
      [ "S3Prefix" Prelude.=: s3Prefix,
        "S3Bucket" Prelude.=: s3Bucket
      ]
