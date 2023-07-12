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
-- Module      : Amazonka.SageMaker.Types.MonitoringStatisticsResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringStatisticsResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The statistics resource for a monitoring job.
--
-- /See:/ 'newMonitoringStatisticsResource' smart constructor.
data MonitoringStatisticsResource = MonitoringStatisticsResource'
  { -- | The Amazon S3 URI for the statistics resource.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringStatisticsResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'monitoringStatisticsResource_s3Uri' - The Amazon S3 URI for the statistics resource.
newMonitoringStatisticsResource ::
  MonitoringStatisticsResource
newMonitoringStatisticsResource =
  MonitoringStatisticsResource'
    { s3Uri =
        Prelude.Nothing
    }

-- | The Amazon S3 URI for the statistics resource.
monitoringStatisticsResource_s3Uri :: Lens.Lens' MonitoringStatisticsResource (Prelude.Maybe Prelude.Text)
monitoringStatisticsResource_s3Uri = Lens.lens (\MonitoringStatisticsResource' {s3Uri} -> s3Uri) (\s@MonitoringStatisticsResource' {} a -> s {s3Uri = a} :: MonitoringStatisticsResource)

instance Data.FromJSON MonitoringStatisticsResource where
  parseJSON =
    Data.withObject
      "MonitoringStatisticsResource"
      ( \x ->
          MonitoringStatisticsResource'
            Prelude.<$> (x Data..:? "S3Uri")
      )

instance
  Prelude.Hashable
    MonitoringStatisticsResource
  where
  hashWithSalt _salt MonitoringStatisticsResource' {..} =
    _salt `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData MonitoringStatisticsResource where
  rnf MonitoringStatisticsResource' {..} =
    Prelude.rnf s3Uri

instance Data.ToJSON MonitoringStatisticsResource where
  toJSON MonitoringStatisticsResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Uri" Data..=) Prelude.<$> s3Uri]
      )
