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
-- Module      : Amazonka.ElasticBeanstalk.Types.SystemStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.SystemStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types.CPUUtilization
import qualified Amazonka.Prelude as Prelude

-- | CPU utilization and load average metrics for an Amazon EC2 instance.
--
-- /See:/ 'newSystemStatus' smart constructor.
data SystemStatus = SystemStatus'
  { -- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For
    -- more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
    loadAverage :: Prelude.Maybe [Prelude.Double],
    -- | CPU utilization metrics for the instance.
    cPUUtilization :: Prelude.Maybe CPUUtilization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadAverage', 'systemStatus_loadAverage' - Load average in the last 1-minute, 5-minute, and 15-minute periods. For
-- more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
--
-- 'cPUUtilization', 'systemStatus_cPUUtilization' - CPU utilization metrics for the instance.
newSystemStatus ::
  SystemStatus
newSystemStatus =
  SystemStatus'
    { loadAverage = Prelude.Nothing,
      cPUUtilization = Prelude.Nothing
    }

-- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For
-- more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
systemStatus_loadAverage :: Lens.Lens' SystemStatus (Prelude.Maybe [Prelude.Double])
systemStatus_loadAverage = Lens.lens (\SystemStatus' {loadAverage} -> loadAverage) (\s@SystemStatus' {} a -> s {loadAverage = a} :: SystemStatus) Prelude.. Lens.mapping Lens.coerced

-- | CPU utilization metrics for the instance.
systemStatus_cPUUtilization :: Lens.Lens' SystemStatus (Prelude.Maybe CPUUtilization)
systemStatus_cPUUtilization = Lens.lens (\SystemStatus' {cPUUtilization} -> cPUUtilization) (\s@SystemStatus' {} a -> s {cPUUtilization = a} :: SystemStatus)

instance Core.FromXML SystemStatus where
  parseXML x =
    SystemStatus'
      Prelude.<$> ( x Core..@? "LoadAverage" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "CPUUtilization")

instance Prelude.Hashable SystemStatus where
  hashWithSalt _salt SystemStatus' {..} =
    _salt `Prelude.hashWithSalt` loadAverage
      `Prelude.hashWithSalt` cPUUtilization

instance Prelude.NFData SystemStatus where
  rnf SystemStatus' {..} =
    Prelude.rnf loadAverage
      `Prelude.seq` Prelude.rnf cPUUtilization
