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
-- Module      : Amazonka.Backup.Types.ReportDeliveryChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ReportDeliveryChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information from your report plan about where to deliver your
-- reports, specifically your Amazon S3 bucket name, S3 key prefix, and the
-- formats of your reports.
--
-- /See:/ 'newReportDeliveryChannel' smart constructor.
data ReportDeliveryChannel = ReportDeliveryChannel'
  { -- | A list of the format of your reports: @CSV@, @JSON@, or both. If not
    -- specified, the default format is @CSV@.
    formats :: Prelude.Maybe [Prelude.Text],
    -- | The prefix for where Backup Audit Manager delivers your reports to
    -- Amazon S3. The prefix is this part of the following path:
    -- s3:\/\/your-bucket-name\/@prefix@\/Backup\/us-west-2\/year\/month\/day\/report-name.
    -- If not specified, there is no prefix.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the S3 bucket that receives your reports.
    s3BucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportDeliveryChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formats', 'reportDeliveryChannel_formats' - A list of the format of your reports: @CSV@, @JSON@, or both. If not
-- specified, the default format is @CSV@.
--
-- 's3KeyPrefix', 'reportDeliveryChannel_s3KeyPrefix' - The prefix for where Backup Audit Manager delivers your reports to
-- Amazon S3. The prefix is this part of the following path:
-- s3:\/\/your-bucket-name\/@prefix@\/Backup\/us-west-2\/year\/month\/day\/report-name.
-- If not specified, there is no prefix.
--
-- 's3BucketName', 'reportDeliveryChannel_s3BucketName' - The unique name of the S3 bucket that receives your reports.
newReportDeliveryChannel ::
  -- | 's3BucketName'
  Prelude.Text ->
  ReportDeliveryChannel
newReportDeliveryChannel pS3BucketName_ =
  ReportDeliveryChannel'
    { formats = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      s3BucketName = pS3BucketName_
    }

-- | A list of the format of your reports: @CSV@, @JSON@, or both. If not
-- specified, the default format is @CSV@.
reportDeliveryChannel_formats :: Lens.Lens' ReportDeliveryChannel (Prelude.Maybe [Prelude.Text])
reportDeliveryChannel_formats = Lens.lens (\ReportDeliveryChannel' {formats} -> formats) (\s@ReportDeliveryChannel' {} a -> s {formats = a} :: ReportDeliveryChannel) Prelude.. Lens.mapping Lens.coerced

-- | The prefix for where Backup Audit Manager delivers your reports to
-- Amazon S3. The prefix is this part of the following path:
-- s3:\/\/your-bucket-name\/@prefix@\/Backup\/us-west-2\/year\/month\/day\/report-name.
-- If not specified, there is no prefix.
reportDeliveryChannel_s3KeyPrefix :: Lens.Lens' ReportDeliveryChannel (Prelude.Maybe Prelude.Text)
reportDeliveryChannel_s3KeyPrefix = Lens.lens (\ReportDeliveryChannel' {s3KeyPrefix} -> s3KeyPrefix) (\s@ReportDeliveryChannel' {} a -> s {s3KeyPrefix = a} :: ReportDeliveryChannel)

-- | The unique name of the S3 bucket that receives your reports.
reportDeliveryChannel_s3BucketName :: Lens.Lens' ReportDeliveryChannel Prelude.Text
reportDeliveryChannel_s3BucketName = Lens.lens (\ReportDeliveryChannel' {s3BucketName} -> s3BucketName) (\s@ReportDeliveryChannel' {} a -> s {s3BucketName = a} :: ReportDeliveryChannel)

instance Data.FromJSON ReportDeliveryChannel where
  parseJSON =
    Data.withObject
      "ReportDeliveryChannel"
      ( \x ->
          ReportDeliveryChannel'
            Prelude.<$> (x Data..:? "Formats" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "S3KeyPrefix")
            Prelude.<*> (x Data..: "S3BucketName")
      )

instance Prelude.Hashable ReportDeliveryChannel where
  hashWithSalt _salt ReportDeliveryChannel' {..} =
    _salt `Prelude.hashWithSalt` formats
      `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` s3BucketName

instance Prelude.NFData ReportDeliveryChannel where
  rnf ReportDeliveryChannel' {..} =
    Prelude.rnf formats
      `Prelude.seq` Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf s3BucketName

instance Data.ToJSON ReportDeliveryChannel where
  toJSON ReportDeliveryChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Formats" Data..=) Prelude.<$> formats,
            ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            Prelude.Just ("S3BucketName" Data..= s3BucketName)
          ]
      )
