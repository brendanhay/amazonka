{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ConstraintSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ConstraintSummary
  ( ConstraintSummary (..)
  -- * Smart constructor
  , mkConstraintSummary
  -- * Lenses
  , csDescription
  , csType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ConstraintDescription as Types
import qualified Network.AWS.ServiceCatalog.Types.ConstraintType as Types

-- | Summary information about a constraint.
--
-- /See:/ 'mkConstraintSummary' smart constructor.
data ConstraintSummary = ConstraintSummary'
  { description :: Core.Maybe Types.ConstraintDescription
    -- ^ The description of the constraint.
  , type' :: Core.Maybe Types.ConstraintType
    -- ^ The type of constraint.
--
--
--     * @LAUNCH@ 
--
--
--     * @NOTIFICATION@ 
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConstraintSummary' value with any optional fields omitted.
mkConstraintSummary
    :: ConstraintSummary
mkConstraintSummary
  = ConstraintSummary'{description = Core.Nothing,
                       type' = Core.Nothing}

-- | The description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' ConstraintSummary (Core.Maybe Types.ConstraintDescription)
csDescription = Lens.field @"description"
{-# INLINEABLE csDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The type of constraint.
--
--
--     * @LAUNCH@ 
--
--
--     * @NOTIFICATION@ 
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@ 
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csType :: Lens.Lens' ConstraintSummary (Core.Maybe Types.ConstraintType)
csType = Lens.field @"type'"
{-# INLINEABLE csType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ConstraintSummary where
        parseJSON
          = Core.withObject "ConstraintSummary" Core.$
              \ x ->
                ConstraintSummary' Core.<$>
                  (x Core..:? "Description") Core.<*> x Core..:? "Type"
