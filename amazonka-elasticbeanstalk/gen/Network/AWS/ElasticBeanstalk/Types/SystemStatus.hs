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
-- Module      : Network.AWS.ElasticBeanstalk.Types.SystemStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SystemStatus where

import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | CPU utilization and load average metrics for an Amazon EC2 instance.
--
-- /See:/ 'newSystemStatus' smart constructor.
data SystemStatus = SystemStatus'
  { -- | CPU utilization metrics for the instance.
    cPUUtilization :: Prelude.Maybe CPUUtilization,
    -- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For
    -- more information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
    loadAverage :: Prelude.Maybe [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { cPUUtilization = Prelude.Nothing,
      loadAverage = Prelude.Nothing
    }

-- | CPU utilization metrics for the instance.
systemStatus_cPUUtilization :: Lens.Lens' SystemStatus (Prelude.Maybe CPUUtilization)
systemStatus_cPUUtilization = Lens.lens (\SystemStatus' {cPUUtilization} -> cPUUtilization) (\s@SystemStatus' {} a -> s {cPUUtilization = a} :: SystemStatus)

-- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For
-- more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
systemStatus_loadAverage :: Lens.Lens' SystemStatus (Prelude.Maybe [Prelude.Double])
systemStatus_loadAverage = Lens.lens (\SystemStatus' {loadAverage} -> loadAverage) (\s@SystemStatus' {} a -> s {loadAverage = a} :: SystemStatus) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML SystemStatus where
  parseXML x =
    SystemStatus'
      Prelude.<$> (x Prelude..@? "CPUUtilization")
      Prelude.<*> ( x Prelude..@? "LoadAverage"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable SystemStatus

instance Prelude.NFData SystemStatus
