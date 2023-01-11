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
-- Module      : Amazonka.SageMaker.Types.MonitoringCsvDatasetFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringCsvDatasetFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the CSV dataset format used when running a monitoring job.
--
-- /See:/ 'newMonitoringCsvDatasetFormat' smart constructor.
data MonitoringCsvDatasetFormat = MonitoringCsvDatasetFormat'
  { -- | Indicates if the CSV data has a header.
    header :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringCsvDatasetFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'header', 'monitoringCsvDatasetFormat_header' - Indicates if the CSV data has a header.
newMonitoringCsvDatasetFormat ::
  MonitoringCsvDatasetFormat
newMonitoringCsvDatasetFormat =
  MonitoringCsvDatasetFormat'
    { header =
        Prelude.Nothing
    }

-- | Indicates if the CSV data has a header.
monitoringCsvDatasetFormat_header :: Lens.Lens' MonitoringCsvDatasetFormat (Prelude.Maybe Prelude.Bool)
monitoringCsvDatasetFormat_header = Lens.lens (\MonitoringCsvDatasetFormat' {header} -> header) (\s@MonitoringCsvDatasetFormat' {} a -> s {header = a} :: MonitoringCsvDatasetFormat)

instance Data.FromJSON MonitoringCsvDatasetFormat where
  parseJSON =
    Data.withObject
      "MonitoringCsvDatasetFormat"
      ( \x ->
          MonitoringCsvDatasetFormat'
            Prelude.<$> (x Data..:? "Header")
      )

instance Prelude.Hashable MonitoringCsvDatasetFormat where
  hashWithSalt _salt MonitoringCsvDatasetFormat' {..} =
    _salt `Prelude.hashWithSalt` header

instance Prelude.NFData MonitoringCsvDatasetFormat where
  rnf MonitoringCsvDatasetFormat' {..} =
    Prelude.rnf header

instance Data.ToJSON MonitoringCsvDatasetFormat where
  toJSON MonitoringCsvDatasetFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Header" Data..=) Prelude.<$> header]
      )
