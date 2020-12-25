{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetHealth
  ( TargetHealth (..),

    -- * Smart constructor
    mkTargetHealth,

    -- * Lenses
    thDescription,
    thReason,
    thState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Description as Types
import qualified Network.AWS.RDS.Types.TargetHealthReason as Types
import qualified Network.AWS.RDS.Types.TargetState as Types

-- | Information about the connection health of an RDS Proxy target.
--
-- /See:/ 'mkTargetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { -- | A description of the health of the RDS Proxy target. If the @State@ is @AVAILABLE@ , a description is not included.
    description :: Core.Maybe Types.Description,
    -- | The reason for the current health @State@ of the RDS Proxy target.
    reason :: Core.Maybe Types.TargetHealthReason,
    -- | The current state of the connection health lifecycle for the RDS Proxy target. The following is a typical lifecycle example for the states of an RDS Proxy target:
    --
    -- @registering@ > @unavailable@ > @available@ > @unavailable@ > @available@
    state :: Core.Maybe Types.TargetState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetHealth' value with any optional fields omitted.
mkTargetHealth ::
  TargetHealth
mkTargetHealth =
  TargetHealth'
    { description = Core.Nothing,
      reason = Core.Nothing,
      state = Core.Nothing
    }

-- | A description of the health of the RDS Proxy target. If the @State@ is @AVAILABLE@ , a description is not included.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thDescription :: Lens.Lens' TargetHealth (Core.Maybe Types.Description)
thDescription = Lens.field @"description"
{-# DEPRECATED thDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The reason for the current health @State@ of the RDS Proxy target.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thReason :: Lens.Lens' TargetHealth (Core.Maybe Types.TargetHealthReason)
thReason = Lens.field @"reason"
{-# DEPRECATED thReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The current state of the connection health lifecycle for the RDS Proxy target. The following is a typical lifecycle example for the states of an RDS Proxy target:
--
-- @registering@ > @unavailable@ > @available@ > @unavailable@ > @available@
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thState :: Lens.Lens' TargetHealth (Core.Maybe Types.TargetState)
thState = Lens.field @"state"
{-# DEPRECATED thState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      Core.<$> (x Core..@? "Description")
      Core.<*> (x Core..@? "Reason")
      Core.<*> (x Core..@? "State")
