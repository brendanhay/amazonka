{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesMonitoring
  ( ScheduledInstancesMonitoring (..),

    -- * Smart constructor
    mkScheduledInstancesMonitoring,

    -- * Lenses
    simEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes whether monitoring is enabled for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesMonitoring' smart constructor.
newtype ScheduledInstancesMonitoring = ScheduledInstancesMonitoring'
  { -- | Indicates whether monitoring is enabled.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesMonitoring' value with any optional fields omitted.
mkScheduledInstancesMonitoring ::
  ScheduledInstancesMonitoring
mkScheduledInstancesMonitoring =
  ScheduledInstancesMonitoring' {enabled = Core.Nothing}

-- | Indicates whether monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simEnabled :: Lens.Lens' ScheduledInstancesMonitoring (Core.Maybe Core.Bool)
simEnabled = Lens.field @"enabled"
{-# DEPRECATED simEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}
