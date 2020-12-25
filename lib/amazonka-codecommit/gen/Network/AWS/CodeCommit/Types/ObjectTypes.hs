{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ObjectTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ObjectTypes
  ( ObjectTypes (..),

    -- * Smart constructor
    mkObjectTypes,

    -- * Lenses
    otBase,
    otDestination,
    otSource,
  )
where

import qualified Network.AWS.CodeCommit.Types.ObjectTypeEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the type of an object in a merge operation.
--
-- /See:/ 'mkObjectTypes' smart constructor.
data ObjectTypes = ObjectTypes'
  { -- | The type of the object in the base commit of the merge.
    base :: Core.Maybe Types.ObjectTypeEnum,
    -- | The type of the object in the destination branch.
    destination :: Core.Maybe Types.ObjectTypeEnum,
    -- | The type of the object in the source branch.
    source :: Core.Maybe Types.ObjectTypeEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ObjectTypes' value with any optional fields omitted.
mkObjectTypes ::
  ObjectTypes
mkObjectTypes =
  ObjectTypes'
    { base = Core.Nothing,
      destination = Core.Nothing,
      source = Core.Nothing
    }

-- | The type of the object in the base commit of the merge.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otBase :: Lens.Lens' ObjectTypes (Core.Maybe Types.ObjectTypeEnum)
otBase = Lens.field @"base"
{-# DEPRECATED otBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The type of the object in the destination branch.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otDestination :: Lens.Lens' ObjectTypes (Core.Maybe Types.ObjectTypeEnum)
otDestination = Lens.field @"destination"
{-# DEPRECATED otDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The type of the object in the source branch.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otSource :: Lens.Lens' ObjectTypes (Core.Maybe Types.ObjectTypeEnum)
otSource = Lens.field @"source"
{-# DEPRECATED otSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON ObjectTypes where
  parseJSON =
    Core.withObject "ObjectTypes" Core.$
      \x ->
        ObjectTypes'
          Core.<$> (x Core..:? "base")
          Core.<*> (x Core..:? "destination")
          Core.<*> (x Core..:? "source")
