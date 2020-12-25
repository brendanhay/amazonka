{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeMetadata
  ( ThingTypeMetadata (..),

    -- * Smart constructor
    mkThingTypeMetadata,

    -- * Lenses
    ttmCreationDate,
    ttmDeprecated,
    ttmDeprecationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.
--
-- /See:/ 'mkThingTypeMetadata' smart constructor.
data ThingTypeMetadata = ThingTypeMetadata'
  { -- | The date and time when the thing type was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
    deprecated :: Core.Maybe Core.Bool,
    -- | The date and time when the thing type was deprecated.
    deprecationDate :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ThingTypeMetadata' value with any optional fields omitted.
mkThingTypeMetadata ::
  ThingTypeMetadata
mkThingTypeMetadata =
  ThingTypeMetadata'
    { creationDate = Core.Nothing,
      deprecated = Core.Nothing,
      deprecationDate = Core.Nothing
    }

-- | The date and time when the thing type was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttmCreationDate :: Lens.Lens' ThingTypeMetadata (Core.Maybe Core.NominalDiffTime)
ttmCreationDate = Lens.field @"creationDate"
{-# DEPRECATED ttmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
--
-- /Note:/ Consider using 'deprecated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttmDeprecated :: Lens.Lens' ThingTypeMetadata (Core.Maybe Core.Bool)
ttmDeprecated = Lens.field @"deprecated"
{-# DEPRECATED ttmDeprecated "Use generic-lens or generic-optics with 'deprecated' instead." #-}

-- | The date and time when the thing type was deprecated.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttmDeprecationDate :: Lens.Lens' ThingTypeMetadata (Core.Maybe Core.NominalDiffTime)
ttmDeprecationDate = Lens.field @"deprecationDate"
{-# DEPRECATED ttmDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

instance Core.FromJSON ThingTypeMetadata where
  parseJSON =
    Core.withObject "ThingTypeMetadata" Core.$
      \x ->
        ThingTypeMetadata'
          Core.<$> (x Core..:? "creationDate")
          Core.<*> (x Core..:? "deprecated")
          Core.<*> (x Core..:? "deprecationDate")
