{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCauseEntity
  ( ErrorRootCauseEntity (..),

    -- * Smart constructor
    mkErrorRootCauseEntity,

    -- * Lenses
    erceExceptions,
    erceName,
    erceRemote,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.Name as Types
import qualified Network.AWS.XRay.Types.RootCauseException as Types

-- | A collection of segments and corresponding subsegments associated to a trace summary error.
--
-- /See:/ 'mkErrorRootCauseEntity' smart constructor.
data ErrorRootCauseEntity = ErrorRootCauseEntity'
  { -- | The types and messages of the exceptions.
    exceptions :: Core.Maybe [Types.RootCauseException],
    -- | The name of the entity.
    name :: Core.Maybe Types.Name,
    -- | A flag that denotes a remote subsegment.
    remote :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorRootCauseEntity' value with any optional fields omitted.
mkErrorRootCauseEntity ::
  ErrorRootCauseEntity
mkErrorRootCauseEntity =
  ErrorRootCauseEntity'
    { exceptions = Core.Nothing,
      name = Core.Nothing,
      remote = Core.Nothing
    }

-- | The types and messages of the exceptions.
--
-- /Note:/ Consider using 'exceptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erceExceptions :: Lens.Lens' ErrorRootCauseEntity (Core.Maybe [Types.RootCauseException])
erceExceptions = Lens.field @"exceptions"
{-# DEPRECATED erceExceptions "Use generic-lens or generic-optics with 'exceptions' instead." #-}

-- | The name of the entity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erceName :: Lens.Lens' ErrorRootCauseEntity (Core.Maybe Types.Name)
erceName = Lens.field @"name"
{-# DEPRECATED erceName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A flag that denotes a remote subsegment.
--
-- /Note:/ Consider using 'remote' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erceRemote :: Lens.Lens' ErrorRootCauseEntity (Core.Maybe Core.Bool)
erceRemote = Lens.field @"remote"
{-# DEPRECATED erceRemote "Use generic-lens or generic-optics with 'remote' instead." #-}

instance Core.FromJSON ErrorRootCauseEntity where
  parseJSON =
    Core.withObject "ErrorRootCauseEntity" Core.$
      \x ->
        ErrorRootCauseEntity'
          Core.<$> (x Core..:? "Exceptions")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Remote")
