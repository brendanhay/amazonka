{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
  ( ModifyRecommendationDetail (..),

    -- * Smart constructor
    mkModifyRecommendationDetail,

    -- * Lenses
    mrdTargetInstances,
  )
where

import qualified Network.AWS.CostExplorer.Types.TargetInstance as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on the modification recommendation.
--
-- /See:/ 'mkModifyRecommendationDetail' smart constructor.
newtype ModifyRecommendationDetail = ModifyRecommendationDetail'
  { -- | Identifies whether this instance type is the AWS default recommendation.
    targetInstances :: Core.Maybe [Types.TargetInstance]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyRecommendationDetail' value with any optional fields omitted.
mkModifyRecommendationDetail ::
  ModifyRecommendationDetail
mkModifyRecommendationDetail =
  ModifyRecommendationDetail' {targetInstances = Core.Nothing}

-- | Identifies whether this instance type is the AWS default recommendation.
--
-- /Note:/ Consider using 'targetInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdTargetInstances :: Lens.Lens' ModifyRecommendationDetail (Core.Maybe [Types.TargetInstance])
mrdTargetInstances = Lens.field @"targetInstances"
{-# DEPRECATED mrdTargetInstances "Use generic-lens or generic-optics with 'targetInstances' instead." #-}

instance Core.FromJSON ModifyRecommendationDetail where
  parseJSON =
    Core.withObject "ModifyRecommendationDetail" Core.$
      \x ->
        ModifyRecommendationDetail' Core.<$> (x Core..:? "TargetInstances")
