{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.OnFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.OnFailure
  ( OnFailure (..),

    -- * Smart constructor
    mkOnFailure,

    -- * Lenses
    ofDestination,
  )
where

import qualified Network.AWS.Lambda.Types.DestinationArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A destination for events that failed processing.
--
-- /See:/ 'mkOnFailure' smart constructor.
newtype OnFailure = OnFailure'
  { -- | The Amazon Resource Name (ARN) of the destination resource.
    destination :: Core.Maybe Types.DestinationArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OnFailure' value with any optional fields omitted.
mkOnFailure ::
  OnFailure
mkOnFailure = OnFailure' {destination = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofDestination :: Lens.Lens' OnFailure (Core.Maybe Types.DestinationArn)
ofDestination = Lens.field @"destination"
{-# DEPRECATED ofDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Core.FromJSON OnFailure where
  toJSON OnFailure {..} =
    Core.object
      (Core.catMaybes [("Destination" Core..=) Core.<$> destination])

instance Core.FromJSON OnFailure where
  parseJSON =
    Core.withObject "OnFailure" Core.$
      \x -> OnFailure' Core.<$> (x Core..:? "Destination")
