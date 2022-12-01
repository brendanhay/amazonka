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
-- Module      : Amazonka.AlexaBusiness.Types.BusinessReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.BusinessReport where

import Amazonka.AlexaBusiness.Types.BusinessReportFailureCode
import Amazonka.AlexaBusiness.Types.BusinessReportS3Location
import Amazonka.AlexaBusiness.Types.BusinessReportStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Usage report with specified parameters.
--
-- /See:/ 'newBusinessReport' smart constructor.
data BusinessReport = BusinessReport'
  { -- | The time of report delivery.
    deliveryTime :: Prelude.Maybe Core.POSIX,
    -- | The failure code.
    failureCode :: Prelude.Maybe BusinessReportFailureCode,
    -- | The download link where a user can download the report.
    downloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The status of the report generation execution (RUNNING, SUCCEEDED, or
    -- FAILED).
    status :: Prelude.Maybe BusinessReportStatus,
    -- | The S3 location of the output reports.
    s3Location :: Prelude.Maybe BusinessReportS3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BusinessReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryTime', 'businessReport_deliveryTime' - The time of report delivery.
--
-- 'failureCode', 'businessReport_failureCode' - The failure code.
--
-- 'downloadUrl', 'businessReport_downloadUrl' - The download link where a user can download the report.
--
-- 'status', 'businessReport_status' - The status of the report generation execution (RUNNING, SUCCEEDED, or
-- FAILED).
--
-- 's3Location', 'businessReport_s3Location' - The S3 location of the output reports.
newBusinessReport ::
  BusinessReport
newBusinessReport =
  BusinessReport'
    { deliveryTime = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      downloadUrl = Prelude.Nothing,
      status = Prelude.Nothing,
      s3Location = Prelude.Nothing
    }

-- | The time of report delivery.
businessReport_deliveryTime :: Lens.Lens' BusinessReport (Prelude.Maybe Prelude.UTCTime)
businessReport_deliveryTime = Lens.lens (\BusinessReport' {deliveryTime} -> deliveryTime) (\s@BusinessReport' {} a -> s {deliveryTime = a} :: BusinessReport) Prelude.. Lens.mapping Core._Time

-- | The failure code.
businessReport_failureCode :: Lens.Lens' BusinessReport (Prelude.Maybe BusinessReportFailureCode)
businessReport_failureCode = Lens.lens (\BusinessReport' {failureCode} -> failureCode) (\s@BusinessReport' {} a -> s {failureCode = a} :: BusinessReport)

-- | The download link where a user can download the report.
businessReport_downloadUrl :: Lens.Lens' BusinessReport (Prelude.Maybe Prelude.Text)
businessReport_downloadUrl = Lens.lens (\BusinessReport' {downloadUrl} -> downloadUrl) (\s@BusinessReport' {} a -> s {downloadUrl = a} :: BusinessReport)

-- | The status of the report generation execution (RUNNING, SUCCEEDED, or
-- FAILED).
businessReport_status :: Lens.Lens' BusinessReport (Prelude.Maybe BusinessReportStatus)
businessReport_status = Lens.lens (\BusinessReport' {status} -> status) (\s@BusinessReport' {} a -> s {status = a} :: BusinessReport)

-- | The S3 location of the output reports.
businessReport_s3Location :: Lens.Lens' BusinessReport (Prelude.Maybe BusinessReportS3Location)
businessReport_s3Location = Lens.lens (\BusinessReport' {s3Location} -> s3Location) (\s@BusinessReport' {} a -> s {s3Location = a} :: BusinessReport)

instance Core.FromJSON BusinessReport where
  parseJSON =
    Core.withObject
      "BusinessReport"
      ( \x ->
          BusinessReport'
            Prelude.<$> (x Core..:? "DeliveryTime")
            Prelude.<*> (x Core..:? "FailureCode")
            Prelude.<*> (x Core..:? "DownloadUrl")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "S3Location")
      )

instance Prelude.Hashable BusinessReport where
  hashWithSalt _salt BusinessReport' {..} =
    _salt `Prelude.hashWithSalt` deliveryTime
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` downloadUrl
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` s3Location

instance Prelude.NFData BusinessReport where
  rnf BusinessReport' {..} =
    Prelude.rnf deliveryTime
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf downloadUrl
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf s3Location
