{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NonCompliantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.NonCompliantSummary
  ( NonCompliantSummary (..)
  -- * Smart constructor
  , mkNonCompliantSummary
  -- * Lenses
  , ncsNonCompliantCount
  , ncsSeveritySummary
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.SeveritySummary as Types

-- | A summary of resources that are not compliant. The summary is organized according to resource type.
--
-- /See:/ 'mkNonCompliantSummary' smart constructor.
data NonCompliantSummary = NonCompliantSummary'
  { nonCompliantCount :: Core.Maybe Core.Int
    -- ^ The total number of compliance items that are not compliant.
  , severitySummary :: Core.Maybe Types.SeveritySummary
    -- ^ A summary of the non-compliance severity by compliance type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NonCompliantSummary' value with any optional fields omitted.
mkNonCompliantSummary
    :: NonCompliantSummary
mkNonCompliantSummary
  = NonCompliantSummary'{nonCompliantCount = Core.Nothing,
                         severitySummary = Core.Nothing}

-- | The total number of compliance items that are not compliant.
--
-- /Note:/ Consider using 'nonCompliantCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncsNonCompliantCount :: Lens.Lens' NonCompliantSummary (Core.Maybe Core.Int)
ncsNonCompliantCount = Lens.field @"nonCompliantCount"
{-# INLINEABLE ncsNonCompliantCount #-}
{-# DEPRECATED nonCompliantCount "Use generic-lens or generic-optics with 'nonCompliantCount' instead"  #-}

-- | A summary of the non-compliance severity by compliance type
--
-- /Note:/ Consider using 'severitySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncsSeveritySummary :: Lens.Lens' NonCompliantSummary (Core.Maybe Types.SeveritySummary)
ncsSeveritySummary = Lens.field @"severitySummary"
{-# INLINEABLE ncsSeveritySummary #-}
{-# DEPRECATED severitySummary "Use generic-lens or generic-optics with 'severitySummary' instead"  #-}

instance Core.FromJSON NonCompliantSummary where
        parseJSON
          = Core.withObject "NonCompliantSummary" Core.$
              \ x ->
                NonCompliantSummary' Core.<$>
                  (x Core..:? "NonCompliantCount") Core.<*>
                    x Core..:? "SeveritySummary"
