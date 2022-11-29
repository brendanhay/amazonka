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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringJsonDatasetFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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

instance Core.FromJSON MonitoringJsonDatasetFormat where
  parseJSON =
    Core.withObject
      "MonitoringJsonDatasetFormat"
      ( \x ->
          MonitoringJsonDatasetFormat'
            Prelude.<$> (x Core..:? "Line")
      )

instance Prelude.Hashable MonitoringJsonDatasetFormat where
  hashWithSalt _salt MonitoringJsonDatasetFormat' {..} =
    _salt `Prelude.hashWithSalt` line

instance Prelude.NFData MonitoringJsonDatasetFormat where
  rnf MonitoringJsonDatasetFormat' {..} =
    Prelude.rnf line

instance Core.ToJSON MonitoringJsonDatasetFormat where
  toJSON MonitoringJsonDatasetFormat' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Line" Core..=) Prelude.<$> line]
      )
