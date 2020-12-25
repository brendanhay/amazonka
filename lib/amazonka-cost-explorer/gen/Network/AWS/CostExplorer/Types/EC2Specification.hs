{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2Specification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2Specification
  ( EC2Specification (..),

    -- * Smart constructor
    mkEC2Specification,

    -- * Lenses
    ecsOfferingClass,
  )
where

import qualified Network.AWS.CostExplorer.Types.OfferingClass as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
--
-- /See:/ 'mkEC2Specification' smart constructor.
newtype EC2Specification = EC2Specification'
  { -- | Whether you want a recommendation for standard or convertible reservations.
    offeringClass :: Core.Maybe Types.OfferingClass
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EC2Specification' value with any optional fields omitted.
mkEC2Specification ::
  EC2Specification
mkEC2Specification =
  EC2Specification' {offeringClass = Core.Nothing}

-- | Whether you want a recommendation for standard or convertible reservations.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsOfferingClass :: Lens.Lens' EC2Specification (Core.Maybe Types.OfferingClass)
ecsOfferingClass = Lens.field @"offeringClass"
{-# DEPRECATED ecsOfferingClass "Use generic-lens or generic-optics with 'offeringClass' instead." #-}

instance Core.FromJSON EC2Specification where
  toJSON EC2Specification {..} =
    Core.object
      (Core.catMaybes [("OfferingClass" Core..=) Core.<$> offeringClass])

instance Core.FromJSON EC2Specification where
  parseJSON =
    Core.withObject "EC2Specification" Core.$
      \x -> EC2Specification' Core.<$> (x Core..:? "OfferingClass")
