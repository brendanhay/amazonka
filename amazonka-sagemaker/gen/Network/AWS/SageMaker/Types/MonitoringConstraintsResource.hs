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
-- Module      : Network.AWS.SageMaker.Types.MonitoringConstraintsResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringConstraintsResource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The constraints resource for a monitoring job.
--
-- /See:/ 'newMonitoringConstraintsResource' smart constructor.
data MonitoringConstraintsResource = MonitoringConstraintsResource'
  { -- | The Amazon S3 URI for the constraints resource.
    s3Uri :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MonitoringConstraintsResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'monitoringConstraintsResource_s3Uri' - The Amazon S3 URI for the constraints resource.
newMonitoringConstraintsResource ::
  MonitoringConstraintsResource
newMonitoringConstraintsResource =
  MonitoringConstraintsResource'
    { s3Uri =
        Core.Nothing
    }

-- | The Amazon S3 URI for the constraints resource.
monitoringConstraintsResource_s3Uri :: Lens.Lens' MonitoringConstraintsResource (Core.Maybe Core.Text)
monitoringConstraintsResource_s3Uri = Lens.lens (\MonitoringConstraintsResource' {s3Uri} -> s3Uri) (\s@MonitoringConstraintsResource' {} a -> s {s3Uri = a} :: MonitoringConstraintsResource)

instance Core.FromJSON MonitoringConstraintsResource where
  parseJSON =
    Core.withObject
      "MonitoringConstraintsResource"
      ( \x ->
          MonitoringConstraintsResource'
            Core.<$> (x Core..:? "S3Uri")
      )

instance Core.Hashable MonitoringConstraintsResource

instance Core.NFData MonitoringConstraintsResource

instance Core.ToJSON MonitoringConstraintsResource where
  toJSON MonitoringConstraintsResource' {..} =
    Core.object
      (Core.catMaybes [("S3Uri" Core..=) Core.<$> s3Uri])
