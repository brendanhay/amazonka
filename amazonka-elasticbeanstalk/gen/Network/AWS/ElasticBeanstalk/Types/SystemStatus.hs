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
-- Module      : Network.AWS.ElasticBeanstalk.Types.SystemStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SystemStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
import qualified Network.AWS.Lens as Lens

-- | CPU utilization and load average metrics for an Amazon EC2 instance.
--
-- /See:/ 'newSystemStatus' smart constructor.
data SystemStatus = SystemStatus'
  { -- | CPU utilization metrics for the instance.
    cPUUtilization :: Core.Maybe CPUUtilization,
    -- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For
    -- more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
    loadAverage :: Core.Maybe [Core.Double]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SystemStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cPUUtilization', 'systemStatus_cPUUtilization' - CPU utilization metrics for the instance.
--
-- 'loadAverage', 'systemStatus_loadAverage' - Load average in the last 1-minute, 5-minute, and 15-minute periods. For
-- more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
newSystemStatus ::
  SystemStatus
newSystemStatus =
  SystemStatus'
    { cPUUtilization = Core.Nothing,
      loadAverage = Core.Nothing
    }

-- | CPU utilization metrics for the instance.
systemStatus_cPUUtilization :: Lens.Lens' SystemStatus (Core.Maybe CPUUtilization)
systemStatus_cPUUtilization = Lens.lens (\SystemStatus' {cPUUtilization} -> cPUUtilization) (\s@SystemStatus' {} a -> s {cPUUtilization = a} :: SystemStatus)

-- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For
-- more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
systemStatus_loadAverage :: Lens.Lens' SystemStatus (Core.Maybe [Core.Double])
systemStatus_loadAverage = Lens.lens (\SystemStatus' {loadAverage} -> loadAverage) (\s@SystemStatus' {} a -> s {loadAverage = a} :: SystemStatus) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML SystemStatus where
  parseXML x =
    SystemStatus'
      Core.<$> (x Core..@? "CPUUtilization")
      Core.<*> ( x Core..@? "LoadAverage" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable SystemStatus

instance Core.NFData SystemStatus
