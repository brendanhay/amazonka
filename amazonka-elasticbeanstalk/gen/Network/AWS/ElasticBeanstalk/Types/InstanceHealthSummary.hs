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
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents summary information about the health of an instance. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
--
-- /See:/ 'newInstanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { -- | __Green.__ An instance is passing health checks and the health agent is
    -- not reporting any problems.
    ok :: Core.Maybe Core.Int,
    -- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no
    -- data on an instance.
    noData :: Core.Maybe Core.Int,
    -- | __Green.__ An operation is in progress on an instance.
    info :: Core.Maybe Core.Int,
    -- | __Red.__ The health agent is reporting a very high number of request
    -- failures or other issues for an instance or environment.
    severe :: Core.Maybe Core.Int,
    -- | __Yellow.__ The health agent is reporting a moderate number of request
    -- failures or other issues for an instance or environment.
    warning :: Core.Maybe Core.Int,
    -- | __Grey.__ An operation is in progress on an instance within the command
    -- timeout.
    pending :: Core.Maybe Core.Int,
    -- | __Red.__ The health agent is reporting a high number of request failures
    -- or other issues for an instance or environment.
    degraded :: Core.Maybe Core.Int,
    -- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an
    -- insufficient amount of data on an instance.
    unknown :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceHealthSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ok', 'instanceHealthSummary_ok' - __Green.__ An instance is passing health checks and the health agent is
-- not reporting any problems.
--
-- 'noData', 'instanceHealthSummary_noData' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no
-- data on an instance.
--
-- 'info', 'instanceHealthSummary_info' - __Green.__ An operation is in progress on an instance.
--
-- 'severe', 'instanceHealthSummary_severe' - __Red.__ The health agent is reporting a very high number of request
-- failures or other issues for an instance or environment.
--
-- 'warning', 'instanceHealthSummary_warning' - __Yellow.__ The health agent is reporting a moderate number of request
-- failures or other issues for an instance or environment.
--
-- 'pending', 'instanceHealthSummary_pending' - __Grey.__ An operation is in progress on an instance within the command
-- timeout.
--
-- 'degraded', 'instanceHealthSummary_degraded' - __Red.__ The health agent is reporting a high number of request failures
-- or other issues for an instance or environment.
--
-- 'unknown', 'instanceHealthSummary_unknown' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an
-- insufficient amount of data on an instance.
newInstanceHealthSummary ::
  InstanceHealthSummary
newInstanceHealthSummary =
  InstanceHealthSummary'
    { ok = Core.Nothing,
      noData = Core.Nothing,
      info = Core.Nothing,
      severe = Core.Nothing,
      warning = Core.Nothing,
      pending = Core.Nothing,
      degraded = Core.Nothing,
      unknown = Core.Nothing
    }

-- | __Green.__ An instance is passing health checks and the health agent is
-- not reporting any problems.
instanceHealthSummary_ok :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_ok = Lens.lens (\InstanceHealthSummary' {ok} -> ok) (\s@InstanceHealthSummary' {} a -> s {ok = a} :: InstanceHealthSummary)

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no
-- data on an instance.
instanceHealthSummary_noData :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_noData = Lens.lens (\InstanceHealthSummary' {noData} -> noData) (\s@InstanceHealthSummary' {} a -> s {noData = a} :: InstanceHealthSummary)

-- | __Green.__ An operation is in progress on an instance.
instanceHealthSummary_info :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_info = Lens.lens (\InstanceHealthSummary' {info} -> info) (\s@InstanceHealthSummary' {} a -> s {info = a} :: InstanceHealthSummary)

-- | __Red.__ The health agent is reporting a very high number of request
-- failures or other issues for an instance or environment.
instanceHealthSummary_severe :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_severe = Lens.lens (\InstanceHealthSummary' {severe} -> severe) (\s@InstanceHealthSummary' {} a -> s {severe = a} :: InstanceHealthSummary)

-- | __Yellow.__ The health agent is reporting a moderate number of request
-- failures or other issues for an instance or environment.
instanceHealthSummary_warning :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_warning = Lens.lens (\InstanceHealthSummary' {warning} -> warning) (\s@InstanceHealthSummary' {} a -> s {warning = a} :: InstanceHealthSummary)

-- | __Grey.__ An operation is in progress on an instance within the command
-- timeout.
instanceHealthSummary_pending :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_pending = Lens.lens (\InstanceHealthSummary' {pending} -> pending) (\s@InstanceHealthSummary' {} a -> s {pending = a} :: InstanceHealthSummary)

-- | __Red.__ The health agent is reporting a high number of request failures
-- or other issues for an instance or environment.
instanceHealthSummary_degraded :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_degraded = Lens.lens (\InstanceHealthSummary' {degraded} -> degraded) (\s@InstanceHealthSummary' {} a -> s {degraded = a} :: InstanceHealthSummary)

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an
-- insufficient amount of data on an instance.
instanceHealthSummary_unknown :: Lens.Lens' InstanceHealthSummary (Core.Maybe Core.Int)
instanceHealthSummary_unknown = Lens.lens (\InstanceHealthSummary' {unknown} -> unknown) (\s@InstanceHealthSummary' {} a -> s {unknown = a} :: InstanceHealthSummary)

instance Core.FromXML InstanceHealthSummary where
  parseXML x =
    InstanceHealthSummary'
      Core.<$> (x Core..@? "Ok")
      Core.<*> (x Core..@? "NoData")
      Core.<*> (x Core..@? "Info")
      Core.<*> (x Core..@? "Severe")
      Core.<*> (x Core..@? "Warning")
      Core.<*> (x Core..@? "Pending")
      Core.<*> (x Core..@? "Degraded")
      Core.<*> (x Core..@? "Unknown")

instance Core.Hashable InstanceHealthSummary

instance Core.NFData InstanceHealthSummary
