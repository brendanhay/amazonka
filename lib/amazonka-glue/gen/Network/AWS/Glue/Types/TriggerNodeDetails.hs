{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerNodeDetails
  ( TriggerNodeDetails (..),

    -- * Smart constructor
    mkTriggerNodeDetails,

    -- * Lenses
    tndTrigger,
  )
where

import qualified Network.AWS.Glue.Types.Trigger as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a Trigger node present in the workflow.
--
-- /See:/ 'mkTriggerNodeDetails' smart constructor.
newtype TriggerNodeDetails = TriggerNodeDetails'
  { -- | The information of the trigger represented by the trigger node.
    trigger :: Core.Maybe Types.Trigger
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TriggerNodeDetails' value with any optional fields omitted.
mkTriggerNodeDetails ::
  TriggerNodeDetails
mkTriggerNodeDetails = TriggerNodeDetails' {trigger = Core.Nothing}

-- | The information of the trigger represented by the trigger node.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tndTrigger :: Lens.Lens' TriggerNodeDetails (Core.Maybe Types.Trigger)
tndTrigger = Lens.field @"trigger"
{-# DEPRECATED tndTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

instance Core.FromJSON TriggerNodeDetails where
  parseJSON =
    Core.withObject "TriggerNodeDetails" Core.$
      \x -> TriggerNodeDetails' Core.<$> (x Core..:? "Trigger")
