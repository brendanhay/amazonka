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
-- Module      : Amazonka.Backup.Types.ReportDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ReportDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information from your report job about your report destination.
--
-- /See:/ 'newReportDestination' smart constructor.
data ReportDestination = ReportDestination'
  { -- | The unique name of the Amazon S3 bucket that receives your reports.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The object key that uniquely identifies your reports in your S3 bucket.
    s3Keys :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketName', 'reportDestination_s3BucketName' - The unique name of the Amazon S3 bucket that receives your reports.
--
-- 's3Keys', 'reportDestination_s3Keys' - The object key that uniquely identifies your reports in your S3 bucket.
newReportDestination ::
  ReportDestination
newReportDestination =
  ReportDestination'
    { s3BucketName = Prelude.Nothing,
      s3Keys = Prelude.Nothing
    }

-- | The unique name of the Amazon S3 bucket that receives your reports.
reportDestination_s3BucketName :: Lens.Lens' ReportDestination (Prelude.Maybe Prelude.Text)
reportDestination_s3BucketName = Lens.lens (\ReportDestination' {s3BucketName} -> s3BucketName) (\s@ReportDestination' {} a -> s {s3BucketName = a} :: ReportDestination)

-- | The object key that uniquely identifies your reports in your S3 bucket.
reportDestination_s3Keys :: Lens.Lens' ReportDestination (Prelude.Maybe [Prelude.Text])
reportDestination_s3Keys = Lens.lens (\ReportDestination' {s3Keys} -> s3Keys) (\s@ReportDestination' {} a -> s {s3Keys = a} :: ReportDestination) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ReportDestination where
  parseJSON =
    Core.withObject
      "ReportDestination"
      ( \x ->
          ReportDestination'
            Prelude.<$> (x Core..:? "S3BucketName")
            Prelude.<*> (x Core..:? "S3Keys" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ReportDestination where
  hashWithSalt _salt ReportDestination' {..} =
    _salt `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3Keys

instance Prelude.NFData ReportDestination where
  rnf ReportDestination' {..} =
    Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3Keys
