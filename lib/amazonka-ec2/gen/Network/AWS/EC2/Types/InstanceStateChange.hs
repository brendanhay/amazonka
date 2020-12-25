{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStateChange
  ( InstanceStateChange (..),

    -- * Smart constructor
    mkInstanceStateChange,

    -- * Lenses
    iscCurrentState,
    iscInstanceId,
    iscPreviousState,
  )
where

import qualified Network.AWS.EC2.Types.InstanceState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance state change.
--
-- /See:/ 'mkInstanceStateChange' smart constructor.
data InstanceStateChange = InstanceStateChange'
  { -- | The current state of the instance.
    currentState :: Core.Maybe Types.InstanceState,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.String,
    -- | The previous state of the instance.
    previousState :: Core.Maybe Types.InstanceState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceStateChange' value with any optional fields omitted.
mkInstanceStateChange ::
  InstanceStateChange
mkInstanceStateChange =
  InstanceStateChange'
    { currentState = Core.Nothing,
      instanceId = Core.Nothing,
      previousState = Core.Nothing
    }

-- | The current state of the instance.
--
-- /Note:/ Consider using 'currentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscCurrentState :: Lens.Lens' InstanceStateChange (Core.Maybe Types.InstanceState)
iscCurrentState = Lens.field @"currentState"
{-# DEPRECATED iscCurrentState "Use generic-lens or generic-optics with 'currentState' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscInstanceId :: Lens.Lens' InstanceStateChange (Core.Maybe Types.String)
iscInstanceId = Lens.field @"instanceId"
{-# DEPRECATED iscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The previous state of the instance.
--
-- /Note:/ Consider using 'previousState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscPreviousState :: Lens.Lens' InstanceStateChange (Core.Maybe Types.InstanceState)
iscPreviousState = Lens.field @"previousState"
{-# DEPRECATED iscPreviousState "Use generic-lens or generic-optics with 'previousState' instead." #-}

instance Core.FromXML InstanceStateChange where
  parseXML x =
    InstanceStateChange'
      Core.<$> (x Core..@? "currentState")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "previousState")
