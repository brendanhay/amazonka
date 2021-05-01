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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Usage report with specified parameters.
--
-- /See:/ 'newBusinessReport' smart constructor.
data BusinessReport = BusinessReport'
  { -- | The download link where a user can download the report.
    downloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The status of the report generation execution (RUNNING, SUCCEEDED, or
    -- FAILED).
    status :: Prelude.Maybe BusinessReportStatus,
    -- | The time of report delivery.
    deliveryTime :: Prelude.Maybe Prelude.POSIX,
    -- | The failure code.
    failureCode :: Prelude.Maybe BusinessReportFailureCode,
    -- | The S3 location of the output reports.
    s3Location :: Prelude.Maybe BusinessReportS3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { downloadUrl = Prelude.Nothing,
      status = Prelude.Nothing,
      deliveryTime = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      s3Location = Prelude.Nothing
    }

-- | The download link where a user can download the report.
businessReport_downloadUrl :: Lens.Lens' BusinessReport (Prelude.Maybe Prelude.Text)
businessReport_downloadUrl = Lens.lens (\BusinessReport' {downloadUrl} -> downloadUrl) (\s@BusinessReport' {} a -> s {downloadUrl = a} :: BusinessReport)

-- | The status of the report generation execution (RUNNING, SUCCEEDED, or
-- FAILED).
businessReport_status :: Lens.Lens' BusinessReport (Prelude.Maybe BusinessReportStatus)
businessReport_status = Lens.lens (\BusinessReport' {status} -> status) (\s@BusinessReport' {} a -> s {status = a} :: BusinessReport)

-- | The time of report delivery.
businessReport_deliveryTime :: Lens.Lens' BusinessReport (Prelude.Maybe Prelude.UTCTime)
businessReport_deliveryTime = Lens.lens (\BusinessReport' {deliveryTime} -> deliveryTime) (\s@BusinessReport' {} a -> s {deliveryTime = a} :: BusinessReport) Prelude.. Lens.mapping Prelude._Time

-- | The failure code.
businessReport_failureCode :: Lens.Lens' BusinessReport (Prelude.Maybe BusinessReportFailureCode)
businessReport_failureCode = Lens.lens (\BusinessReport' {failureCode} -> failureCode) (\s@BusinessReport' {} a -> s {failureCode = a} :: BusinessReport)

-- | The S3 location of the output reports.
businessReport_s3Location :: Lens.Lens' BusinessReport (Prelude.Maybe BusinessReportS3Location)
businessReport_s3Location = Lens.lens (\BusinessReport' {s3Location} -> s3Location) (\s@BusinessReport' {} a -> s {s3Location = a} :: BusinessReport)

instance Prelude.FromJSON BusinessReport where
  parseJSON =
    Prelude.withObject
      "BusinessReport"
      ( \x ->
          BusinessReport'
            Prelude.<$> (x Prelude..:? "DownloadUrl")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "DeliveryTime")
            Prelude.<*> (x Prelude..:? "FailureCode")
            Prelude.<*> (x Prelude..:? "S3Location")
      )

instance Prelude.Hashable BusinessReport

instance Prelude.NFData BusinessReport
