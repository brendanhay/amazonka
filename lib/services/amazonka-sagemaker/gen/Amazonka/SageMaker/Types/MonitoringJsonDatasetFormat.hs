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
-- Module      : Amazonka.SageMaker.Types.MonitoringJsonDatasetFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringJsonDatasetFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the JSON dataset format used when running a monitoring job.
--
-- /See:/ 'newMonitoringJsonDatasetFormat' smart constructor.
data MonitoringJsonDatasetFormat = MonitoringJsonDatasetFormat'
  { -- | Indicates if the file should be read as a json object per line.
    line :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringJsonDatasetFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'line', 'monitoringJsonDatasetFormat_line' - Indicates if the file should be read as a json object per line.
newMonitoringJsonDatasetFormat ::
  MonitoringJsonDatasetFormat
newMonitoringJsonDatasetFormat =
  MonitoringJsonDatasetFormat'
    { line =
        Prelude.Nothing
    }

-- | Indicates if the file should be read as a json object per line.
monitoringJsonDatasetFormat_line :: Lens.Lens' MonitoringJsonDatasetFormat (Prelude.Maybe Prelude.Bool)
monitoringJsonDatasetFormat_line = Lens.lens (\MonitoringJsonDatasetFormat' {line} -> line) (\s@MonitoringJsonDatasetFormat' {} a -> s {line = a} :: MonitoringJsonDatasetFormat)

instance Data.FromJSON MonitoringJsonDatasetFormat where
  parseJSON =
    Data.withObject
      "MonitoringJsonDatasetFormat"
      ( \x ->
          MonitoringJsonDatasetFormat'
            Prelude.<$> (x Data..:? "Line")
      )

instance Prelude.Hashable MonitoringJsonDatasetFormat where
  hashWithSalt _salt MonitoringJsonDatasetFormat' {..} =
    _salt `Prelude.hashWithSalt` line

instance Prelude.NFData MonitoringJsonDatasetFormat where
  rnf MonitoringJsonDatasetFormat' {..} =
    Prelude.rnf line

instance Data.ToJSON MonitoringJsonDatasetFormat where
  toJSON MonitoringJsonDatasetFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Line" Data..=) Prelude.<$> line]
      )
