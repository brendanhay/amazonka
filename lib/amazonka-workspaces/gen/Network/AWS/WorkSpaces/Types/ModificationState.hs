{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ModificationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ModificationState
  ( ModificationState (..)
  -- * Smart constructor
  , mkModificationState
  -- * Lenses
  , msResource
  , msState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.ModificationResourceEnum as Types
import qualified Network.AWS.WorkSpaces.Types.ModificationStateEnum as Types

-- | Describes a WorkSpace modification.
--
-- /See:/ 'mkModificationState' smart constructor.
data ModificationState = ModificationState'
  { resource :: Core.Maybe Types.ModificationResourceEnum
    -- ^ The resource.
  , state :: Core.Maybe Types.ModificationStateEnum
    -- ^ The modification state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModificationState' value with any optional fields omitted.
mkModificationState
    :: ModificationState
mkModificationState
  = ModificationState'{resource = Core.Nothing, state = Core.Nothing}

-- | The resource.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msResource :: Lens.Lens' ModificationState (Core.Maybe Types.ModificationResourceEnum)
msResource = Lens.field @"resource"
{-# INLINEABLE msResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

-- | The modification state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msState :: Lens.Lens' ModificationState (Core.Maybe Types.ModificationStateEnum)
msState = Lens.field @"state"
{-# INLINEABLE msState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON ModificationState where
        parseJSON
          = Core.withObject "ModificationState" Core.$
              \ x ->
                ModificationState' Core.<$>
                  (x Core..:? "Resource") Core.<*> x Core..:? "State"
