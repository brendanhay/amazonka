{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
  ( RunInstancesMonitoringEnabled (..),

    -- * Smart constructor
    mkRunInstancesMonitoringEnabled,

    -- * Lenses
    rimeEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the monitoring of an instance.
--
-- /See:/ 'mkRunInstancesMonitoringEnabled' smart constructor.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
    enabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RunInstancesMonitoringEnabled' value with any optional fields omitted.
mkRunInstancesMonitoringEnabled ::
  -- | 'enabled'
  Core.Bool ->
  RunInstancesMonitoringEnabled
mkRunInstancesMonitoringEnabled enabled =
  RunInstancesMonitoringEnabled' {enabled}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimeEnabled :: Lens.Lens' RunInstancesMonitoringEnabled Core.Bool
rimeEnabled = Lens.field @"enabled"
{-# DEPRECATED rimeEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromXML RunInstancesMonitoringEnabled where
  parseXML x =
    RunInstancesMonitoringEnabled' Core.<$> (x Core..@ "enabled")
