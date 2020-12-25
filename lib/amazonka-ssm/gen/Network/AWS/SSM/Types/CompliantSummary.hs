{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CompliantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CompliantSummary
  ( CompliantSummary (..),

    -- * Smart constructor
    mkCompliantSummary,

    -- * Lenses
    csCompliantCount,
    csSeveritySummary,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.SeveritySummary as Types

-- | A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.
--
-- /See:/ 'mkCompliantSummary' smart constructor.
data CompliantSummary = CompliantSummary'
  { -- | The total number of resources that are compliant.
    compliantCount :: Core.Maybe Core.Int,
    -- | A summary of the compliance severity by compliance type.
    severitySummary :: Core.Maybe Types.SeveritySummary
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompliantSummary' value with any optional fields omitted.
mkCompliantSummary ::
  CompliantSummary
mkCompliantSummary =
  CompliantSummary'
    { compliantCount = Core.Nothing,
      severitySummary = Core.Nothing
    }

-- | The total number of resources that are compliant.
--
-- /Note:/ Consider using 'compliantCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCompliantCount :: Lens.Lens' CompliantSummary (Core.Maybe Core.Int)
csCompliantCount = Lens.field @"compliantCount"
{-# DEPRECATED csCompliantCount "Use generic-lens or generic-optics with 'compliantCount' instead." #-}

-- | A summary of the compliance severity by compliance type.
--
-- /Note:/ Consider using 'severitySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSeveritySummary :: Lens.Lens' CompliantSummary (Core.Maybe Types.SeveritySummary)
csSeveritySummary = Lens.field @"severitySummary"
{-# DEPRECATED csSeveritySummary "Use generic-lens or generic-optics with 'severitySummary' instead." #-}

instance Core.FromJSON CompliantSummary where
  parseJSON =
    Core.withObject "CompliantSummary" Core.$
      \x ->
        CompliantSummary'
          Core.<$> (x Core..:? "CompliantCount")
          Core.<*> (x Core..:? "SeveritySummary")
