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
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesMonitoring where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes whether monitoring is enabled for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesMonitoring' smart constructor.
data ScheduledInstancesMonitoring = ScheduledInstancesMonitoring'
  { -- | Indicates whether monitoring is enabled.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledInstancesMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'scheduledInstancesMonitoring_enabled' - Indicates whether monitoring is enabled.
newScheduledInstancesMonitoring ::
  ScheduledInstancesMonitoring
newScheduledInstancesMonitoring =
  ScheduledInstancesMonitoring'
    { enabled =
        Core.Nothing
    }

-- | Indicates whether monitoring is enabled.
scheduledInstancesMonitoring_enabled :: Lens.Lens' ScheduledInstancesMonitoring (Core.Maybe Core.Bool)
scheduledInstancesMonitoring_enabled = Lens.lens (\ScheduledInstancesMonitoring' {enabled} -> enabled) (\s@ScheduledInstancesMonitoring' {} a -> s {enabled = a} :: ScheduledInstancesMonitoring)

instance Core.Hashable ScheduledInstancesMonitoring

instance Core.NFData ScheduledInstancesMonitoring

instance Core.ToQuery ScheduledInstancesMonitoring where
  toQuery ScheduledInstancesMonitoring' {..} =
    Core.mconcat ["Enabled" Core.=: enabled]
