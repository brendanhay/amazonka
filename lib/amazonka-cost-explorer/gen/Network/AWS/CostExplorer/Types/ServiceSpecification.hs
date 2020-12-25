{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ServiceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ServiceSpecification
  ( ServiceSpecification (..),

    -- * Smart constructor
    mkServiceSpecification,

    -- * Lenses
    ssEC2Specification,
  )
where

import qualified Network.AWS.CostExplorer.Types.EC2Specification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Hardware specifications for the service that you want recommendations for.
--
-- /See:/ 'mkServiceSpecification' smart constructor.
newtype ServiceSpecification = ServiceSpecification'
  { -- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
    eC2Specification :: Core.Maybe Types.EC2Specification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceSpecification' value with any optional fields omitted.
mkServiceSpecification ::
  ServiceSpecification
mkServiceSpecification =
  ServiceSpecification' {eC2Specification = Core.Nothing}

-- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
--
-- /Note:/ Consider using 'eC2Specification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEC2Specification :: Lens.Lens' ServiceSpecification (Core.Maybe Types.EC2Specification)
ssEC2Specification = Lens.field @"eC2Specification"
{-# DEPRECATED ssEC2Specification "Use generic-lens or generic-optics with 'eC2Specification' instead." #-}

instance Core.FromJSON ServiceSpecification where
  toJSON ServiceSpecification {..} =
    Core.object
      ( Core.catMaybes
          [("EC2Specification" Core..=) Core.<$> eC2Specification]
      )

instance Core.FromJSON ServiceSpecification where
  parseJSON =
    Core.withObject "ServiceSpecification" Core.$
      \x ->
        ServiceSpecification' Core.<$> (x Core..:? "EC2Specification")
