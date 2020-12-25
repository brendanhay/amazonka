{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Monitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Monitoring
  ( Monitoring (..),

    -- * Smart constructor
    mkMonitoring,

    -- * Lenses
    mState,
  )
where

import qualified Network.AWS.EC2.Types.MonitoringState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the monitoring of an instance.
--
-- /See:/ 'mkMonitoring' smart constructor.
newtype Monitoring = Monitoring'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
    state :: Core.Maybe Types.MonitoringState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Monitoring' value with any optional fields omitted.
mkMonitoring ::
  Monitoring
mkMonitoring = Monitoring' {state = Core.Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mState :: Lens.Lens' Monitoring (Core.Maybe Types.MonitoringState)
mState = Lens.field @"state"
{-# DEPRECATED mState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromXML Monitoring where
  parseXML x = Monitoring' Core.<$> (x Core..@? "state")
