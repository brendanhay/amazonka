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

import Network.AWS.CostExplorer.Types.TargetInstance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on the modification recommendation.
--
-- /See:/ 'mkModifyRecommendationDetail' smart constructor.
newtype ModifyRecommendationDetail = ModifyRecommendationDetail'
  { -- | Identifies whether this instance type is the AWS default recommendation.
    targetInstances :: Lude.Maybe [TargetInstance]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyRecommendationDetail' with the minimum fields required to make a request.
--
-- * 'targetInstances' - Identifies whether this instance type is the AWS default recommendation.
mkModifyRecommendationDetail ::
  ModifyRecommendationDetail
mkModifyRecommendationDetail =
  ModifyRecommendationDetail' {targetInstances = Lude.Nothing}

-- | Identifies whether this instance type is the AWS default recommendation.
--
-- /Note:/ Consider using 'targetInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdTargetInstances :: Lens.Lens' ModifyRecommendationDetail (Lude.Maybe [TargetInstance])
mrdTargetInstances = Lens.lens (targetInstances :: ModifyRecommendationDetail -> Lude.Maybe [TargetInstance]) (\s a -> s {targetInstances = a} :: ModifyRecommendationDetail)
{-# DEPRECATED mrdTargetInstances "Use generic-lens or generic-optics with 'targetInstances' instead." #-}

instance Lude.FromJSON ModifyRecommendationDetail where
  parseJSON =
    Lude.withObject
      "ModifyRecommendationDetail"
      ( \x ->
          ModifyRecommendationDetail'
            Lude.<$> (x Lude..:? "TargetInstances" Lude..!= Lude.mempty)
      )
