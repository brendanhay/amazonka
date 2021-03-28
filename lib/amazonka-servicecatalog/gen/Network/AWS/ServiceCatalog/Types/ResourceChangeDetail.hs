{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
  ( ResourceChangeDetail (..)
  -- * Smart constructor
  , mkResourceChangeDetail
  -- * Lenses
  , rcdCausingEntity
  , rcdEvaluation
  , rcdTarget
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.CausingEntity as Types
import qualified Network.AWS.ServiceCatalog.Types.EvaluationType as Types
import qualified Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition as Types

-- | Information about a change to a resource attribute.
--
-- /See:/ 'mkResourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { causingEntity :: Core.Maybe Types.CausingEntity
    -- ^ The ID of the entity that caused the change.
  , evaluation :: Core.Maybe Types.EvaluationType
    -- ^ For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
  , target :: Core.Maybe Types.ResourceTargetDefinition
    -- ^ Information about the resource attribute to be modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceChangeDetail' value with any optional fields omitted.
mkResourceChangeDetail
    :: ResourceChangeDetail
mkResourceChangeDetail
  = ResourceChangeDetail'{causingEntity = Core.Nothing,
                          evaluation = Core.Nothing, target = Core.Nothing}

-- | The ID of the entity that caused the change.
--
-- /Note:/ Consider using 'causingEntity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdCausingEntity :: Lens.Lens' ResourceChangeDetail (Core.Maybe Types.CausingEntity)
rcdCausingEntity = Lens.field @"causingEntity"
{-# INLINEABLE rcdCausingEntity #-}
{-# DEPRECATED causingEntity "Use generic-lens or generic-optics with 'causingEntity' instead"  #-}

-- | For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
--
-- /Note:/ Consider using 'evaluation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdEvaluation :: Lens.Lens' ResourceChangeDetail (Core.Maybe Types.EvaluationType)
rcdEvaluation = Lens.field @"evaluation"
{-# INLINEABLE rcdEvaluation #-}
{-# DEPRECATED evaluation "Use generic-lens or generic-optics with 'evaluation' instead"  #-}

-- | Information about the resource attribute to be modified.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdTarget :: Lens.Lens' ResourceChangeDetail (Core.Maybe Types.ResourceTargetDefinition)
rcdTarget = Lens.field @"target"
{-# INLINEABLE rcdTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

instance Core.FromJSON ResourceChangeDetail where
        parseJSON
          = Core.withObject "ResourceChangeDetail" Core.$
              \ x ->
                ResourceChangeDetail' Core.<$>
                  (x Core..:? "CausingEntity") Core.<*> x Core..:? "Evaluation"
                    Core.<*> x Core..:? "Target"
