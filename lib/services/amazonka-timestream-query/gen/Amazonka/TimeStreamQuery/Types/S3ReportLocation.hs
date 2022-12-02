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
-- Module      : Amazonka.TimeStreamQuery.Types.S3ReportLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.S3ReportLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | S3 report location for the scheduled query run.
--
-- /See:/ 'newS3ReportLocation' smart constructor.
data S3ReportLocation = S3ReportLocation'
  { -- | S3 key.
    objectKey :: Prelude.Maybe Prelude.Text,
    -- | S3 bucket name.
    bucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ReportLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectKey', 's3ReportLocation_objectKey' - S3 key.
--
-- 'bucketName', 's3ReportLocation_bucketName' - S3 bucket name.
newS3ReportLocation ::
  S3ReportLocation
newS3ReportLocation =
  S3ReportLocation'
    { objectKey = Prelude.Nothing,
      bucketName = Prelude.Nothing
    }

-- | S3 key.
s3ReportLocation_objectKey :: Lens.Lens' S3ReportLocation (Prelude.Maybe Prelude.Text)
s3ReportLocation_objectKey = Lens.lens (\S3ReportLocation' {objectKey} -> objectKey) (\s@S3ReportLocation' {} a -> s {objectKey = a} :: S3ReportLocation)

-- | S3 bucket name.
s3ReportLocation_bucketName :: Lens.Lens' S3ReportLocation (Prelude.Maybe Prelude.Text)
s3ReportLocation_bucketName = Lens.lens (\S3ReportLocation' {bucketName} -> bucketName) (\s@S3ReportLocation' {} a -> s {bucketName = a} :: S3ReportLocation)

instance Data.FromJSON S3ReportLocation where
  parseJSON =
    Data.withObject
      "S3ReportLocation"
      ( \x ->
          S3ReportLocation'
            Prelude.<$> (x Data..:? "ObjectKey")
            Prelude.<*> (x Data..:? "BucketName")
      )

instance Prelude.Hashable S3ReportLocation where
  hashWithSalt _salt S3ReportLocation' {..} =
    _salt `Prelude.hashWithSalt` objectKey
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData S3ReportLocation where
  rnf S3ReportLocation' {..} =
    Prelude.rnf objectKey
      `Prelude.seq` Prelude.rnf bucketName
