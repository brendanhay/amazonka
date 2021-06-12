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
-- Module      : Network.AWS.SageMaker.Types.MonitoringStatisticsResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringStatisticsResource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The statistics resource for a monitoring job.
--
-- /See:/ 'newMonitoringStatisticsResource' smart constructor.
data MonitoringStatisticsResource = MonitoringStatisticsResource'
  { -- | The Amazon S3 URI for the statistics resource.
    s3Uri :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  MonitoringStatisticsResource' {s3Uri = Core.Nothing}

-- | The Amazon S3 URI for the statistics resource.
monitoringStatisticsResource_s3Uri :: Lens.Lens' MonitoringStatisticsResource (Core.Maybe Core.Text)
monitoringStatisticsResource_s3Uri = Lens.lens (\MonitoringStatisticsResource' {s3Uri} -> s3Uri) (\s@MonitoringStatisticsResource' {} a -> s {s3Uri = a} :: MonitoringStatisticsResource)

instance Core.FromJSON MonitoringStatisticsResource where
  parseJSON =
    Core.withObject
      "MonitoringStatisticsResource"
      ( \x ->
          MonitoringStatisticsResource'
            Core.<$> (x Core..:? "S3Uri")
      )

instance Core.Hashable MonitoringStatisticsResource

instance Core.NFData MonitoringStatisticsResource

instance Core.ToJSON MonitoringStatisticsResource where
  toJSON MonitoringStatisticsResource' {..} =
    Core.object
      (Core.catMaybes [("S3Uri" Core..=) Core.<$> s3Uri])
