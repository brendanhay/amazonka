{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
  ( MapStateStartedEventDetails (..),

    -- * Smart constructor
    mkMapStateStartedEventDetails,

    -- * Lenses
    mssedLength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a Map state that was started.
--
-- /See:/ 'mkMapStateStartedEventDetails' smart constructor.
newtype MapStateStartedEventDetails = MapStateStartedEventDetails'
  { -- | The size of the array for Map state iterations.
    length :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MapStateStartedEventDetails' value with any optional fields omitted.
mkMapStateStartedEventDetails ::
  MapStateStartedEventDetails
mkMapStateStartedEventDetails =
  MapStateStartedEventDetails' {length = Core.Nothing}

-- | The size of the array for Map state iterations.
--
-- /Note:/ Consider using 'length' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssedLength :: Lens.Lens' MapStateStartedEventDetails (Core.Maybe Core.Natural)
mssedLength = Lens.field @"length"
{-# DEPRECATED mssedLength "Use generic-lens or generic-optics with 'length' instead." #-}

instance Core.FromJSON MapStateStartedEventDetails where
  parseJSON =
    Core.withObject "MapStateStartedEventDetails" Core.$
      \x -> MapStateStartedEventDetails' Core.<$> (x Core..:? "length")
