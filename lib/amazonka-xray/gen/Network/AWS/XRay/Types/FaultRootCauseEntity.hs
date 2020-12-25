{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseEntity
  ( FaultRootCauseEntity (..),

    -- * Smart constructor
    mkFaultRootCauseEntity,

    -- * Lenses
    frceExceptions,
    frceName,
    frceRemote,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.RootCauseException as Types
import qualified Network.AWS.XRay.Types.String as Types

-- | A collection of segments and corresponding subsegments associated to a trace summary fault error.
--
-- /See:/ 'mkFaultRootCauseEntity' smart constructor.
data FaultRootCauseEntity = FaultRootCauseEntity'
  { -- | The types and messages of the exceptions.
    exceptions :: Core.Maybe [Types.RootCauseException],
    -- | The name of the entity.
    name :: Core.Maybe Types.String,
    -- | A flag that denotes a remote subsegment.
    remote :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaultRootCauseEntity' value with any optional fields omitted.
mkFaultRootCauseEntity ::
  FaultRootCauseEntity
mkFaultRootCauseEntity =
  FaultRootCauseEntity'
    { exceptions = Core.Nothing,
      name = Core.Nothing,
      remote = Core.Nothing
    }

-- | The types and messages of the exceptions.
--
-- /Note:/ Consider using 'exceptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceExceptions :: Lens.Lens' FaultRootCauseEntity (Core.Maybe [Types.RootCauseException])
frceExceptions = Lens.field @"exceptions"
{-# DEPRECATED frceExceptions "Use generic-lens or generic-optics with 'exceptions' instead." #-}

-- | The name of the entity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceName :: Lens.Lens' FaultRootCauseEntity (Core.Maybe Types.String)
frceName = Lens.field @"name"
{-# DEPRECATED frceName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A flag that denotes a remote subsegment.
--
-- /Note:/ Consider using 'remote' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceRemote :: Lens.Lens' FaultRootCauseEntity (Core.Maybe Core.Bool)
frceRemote = Lens.field @"remote"
{-# DEPRECATED frceRemote "Use generic-lens or generic-optics with 'remote' instead." #-}

instance Core.FromJSON FaultRootCauseEntity where
  parseJSON =
    Core.withObject "FaultRootCauseEntity" Core.$
      \x ->
        FaultRootCauseEntity'
          Core.<$> (x Core..:? "Exceptions")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Remote")
