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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReport where

import Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
import Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
import Network.AWS.AlexaBusiness.Types.BusinessReportStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Usage report with specified parameters.
--
-- /See:/ 'newBusinessReport' smart constructor.
data BusinessReport = BusinessReport'
  { -- | The download link where a user can download the report.
    downloadUrl :: Core.Maybe Core.Text,
    -- | The status of the report generation execution (RUNNING, SUCCEEDED, or
    -- FAILED).
    status :: Core.Maybe BusinessReportStatus,
    -- | The time of report delivery.
    deliveryTime :: Core.Maybe Core.POSIX,
    -- | The failure code.
    failureCode :: Core.Maybe BusinessReportFailureCode,
    -- | The S3 location of the output reports.
    s3Location :: Core.Maybe BusinessReportS3Location
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BusinessReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadUrl', 'businessReport_downloadUrl' - The download link where a user can download the report.
--
-- 'status', 'businessReport_status' - The status of the report generation execution (RUNNING, SUCCEEDED, or
-- FAILED).
--
-- 'deliveryTime', 'businessReport_deliveryTime' - The time of report delivery.
--
-- 'failureCode', 'businessReport_failureCode' - The failure code.
--
-- 's3Location', 'businessReport_s3Location' - The S3 location of the output reports.
newBusinessReport ::
  BusinessReport
newBusinessReport =
  BusinessReport'
    { downloadUrl = Core.Nothing,
      status = Core.Nothing,
      deliveryTime = Core.Nothing,
      failureCode = Core.Nothing,
      s3Location = Core.Nothing
    }

-- | The download link where a user can download the report.
businessReport_downloadUrl :: Lens.Lens' BusinessReport (Core.Maybe Core.Text)
businessReport_downloadUrl = Lens.lens (\BusinessReport' {downloadUrl} -> downloadUrl) (\s@BusinessReport' {} a -> s {downloadUrl = a} :: BusinessReport)

-- | The status of the report generation execution (RUNNING, SUCCEEDED, or
-- FAILED).
businessReport_status :: Lens.Lens' BusinessReport (Core.Maybe BusinessReportStatus)
businessReport_status = Lens.lens (\BusinessReport' {status} -> status) (\s@BusinessReport' {} a -> s {status = a} :: BusinessReport)

-- | The time of report delivery.
businessReport_deliveryTime :: Lens.Lens' BusinessReport (Core.Maybe Core.UTCTime)
businessReport_deliveryTime = Lens.lens (\BusinessReport' {deliveryTime} -> deliveryTime) (\s@BusinessReport' {} a -> s {deliveryTime = a} :: BusinessReport) Core.. Lens.mapping Core._Time

-- | The failure code.
businessReport_failureCode :: Lens.Lens' BusinessReport (Core.Maybe BusinessReportFailureCode)
businessReport_failureCode = Lens.lens (\BusinessReport' {failureCode} -> failureCode) (\s@BusinessReport' {} a -> s {failureCode = a} :: BusinessReport)

-- | The S3 location of the output reports.
businessReport_s3Location :: Lens.Lens' BusinessReport (Core.Maybe BusinessReportS3Location)
businessReport_s3Location = Lens.lens (\BusinessReport' {s3Location} -> s3Location) (\s@BusinessReport' {} a -> s {s3Location = a} :: BusinessReport)

instance Core.FromJSON BusinessReport where
  parseJSON =
    Core.withObject
      "BusinessReport"
      ( \x ->
          BusinessReport'
            Core.<$> (x Core..:? "DownloadUrl")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "DeliveryTime")
            Core.<*> (x Core..:? "FailureCode")
            Core.<*> (x Core..:? "S3Location")
      )

instance Core.Hashable BusinessReport

instance Core.NFData BusinessReport
