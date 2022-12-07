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
-- Module      : Amazonka.TimeStreamQuery.Types.ErrorReportLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ErrorReportLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.S3ReportLocation

-- | This contains the location of the error report for a single scheduled
-- query call.
--
-- /See:/ 'newErrorReportLocation' smart constructor.
data ErrorReportLocation = ErrorReportLocation'
  { -- | The S3 location where error reports are written.
    s3ReportLocation :: Prelude.Maybe S3ReportLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorReportLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ReportLocation', 'errorReportLocation_s3ReportLocation' - The S3 location where error reports are written.
newErrorReportLocation ::
  ErrorReportLocation
newErrorReportLocation =
  ErrorReportLocation'
    { s3ReportLocation =
        Prelude.Nothing
    }

-- | The S3 location where error reports are written.
errorReportLocation_s3ReportLocation :: Lens.Lens' ErrorReportLocation (Prelude.Maybe S3ReportLocation)
errorReportLocation_s3ReportLocation = Lens.lens (\ErrorReportLocation' {s3ReportLocation} -> s3ReportLocation) (\s@ErrorReportLocation' {} a -> s {s3ReportLocation = a} :: ErrorReportLocation)

instance Data.FromJSON ErrorReportLocation where
  parseJSON =
    Data.withObject
      "ErrorReportLocation"
      ( \x ->
          ErrorReportLocation'
            Prelude.<$> (x Data..:? "S3ReportLocation")
      )

instance Prelude.Hashable ErrorReportLocation where
  hashWithSalt _salt ErrorReportLocation' {..} =
    _salt `Prelude.hashWithSalt` s3ReportLocation

instance Prelude.NFData ErrorReportLocation where
  rnf ErrorReportLocation' {..} =
    Prelude.rnf s3ReportLocation
