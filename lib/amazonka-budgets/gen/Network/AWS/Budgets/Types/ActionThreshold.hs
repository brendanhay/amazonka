{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionThreshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.ActionThreshold
  ( ActionThreshold (..)
  -- * Smart constructor
  , mkActionThreshold
  -- * Lenses
  , atActionThresholdValue
  , atActionThresholdType
  ) where

import qualified Network.AWS.Budgets.Types.ThresholdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The trigger threshold of the action. 
--
-- /See:/ 'mkActionThreshold' smart constructor.
data ActionThreshold = ActionThreshold'
  { actionThresholdValue :: Core.Double
  , actionThresholdType :: Types.ThresholdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionThreshold' value with any optional fields omitted.
mkActionThreshold
    :: Core.Double -- ^ 'actionThresholdValue'
    -> Types.ThresholdType -- ^ 'actionThresholdType'
    -> ActionThreshold
mkActionThreshold actionThresholdValue actionThresholdType
  = ActionThreshold'{actionThresholdValue, actionThresholdType}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThresholdValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atActionThresholdValue :: Lens.Lens' ActionThreshold Core.Double
atActionThresholdValue = Lens.field @"actionThresholdValue"
{-# INLINEABLE atActionThresholdValue #-}
{-# DEPRECATED actionThresholdValue "Use generic-lens or generic-optics with 'actionThresholdValue' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThresholdType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atActionThresholdType :: Lens.Lens' ActionThreshold Types.ThresholdType
atActionThresholdType = Lens.field @"actionThresholdType"
{-# INLINEABLE atActionThresholdType #-}
{-# DEPRECATED actionThresholdType "Use generic-lens or generic-optics with 'actionThresholdType' instead"  #-}

instance Core.FromJSON ActionThreshold where
        toJSON ActionThreshold{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ActionThresholdValue" Core..= actionThresholdValue),
                  Core.Just ("ActionThresholdType" Core..= actionThresholdType)])

instance Core.FromJSON ActionThreshold where
        parseJSON
          = Core.withObject "ActionThreshold" Core.$
              \ x ->
                ActionThreshold' Core.<$>
                  (x Core..: "ActionThresholdValue") Core.<*>
                    x Core..: "ActionThresholdType"
