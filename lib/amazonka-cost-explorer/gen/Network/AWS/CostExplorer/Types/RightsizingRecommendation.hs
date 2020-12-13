{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendation
  ( RightsizingRecommendation (..),

    -- * Smart constructor
    mkRightsizingRecommendation,

    -- * Lenses
    rrAccountId,
    rrModifyRecommendationDetail,
    rrCurrentInstance,
    rrRightsizingType,
    rrTerminateRecommendationDetail,
  )
where

import Network.AWS.CostExplorer.Types.CurrentInstance
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
import Network.AWS.CostExplorer.Types.RightsizingType
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Recommendations to rightsize resources.
--
-- /See:/ 'mkRightsizingRecommendation' smart constructor.
data RightsizingRecommendation = RightsizingRecommendation'
  { -- | The account that this recommendation is for.
    accountId :: Lude.Maybe Lude.Text,
    -- | Details for modification recommendations.
    modifyRecommendationDetail :: Lude.Maybe ModifyRecommendationDetail,
    -- | Context regarding the current instance.
    currentInstance :: Lude.Maybe CurrentInstance,
    -- | Recommendation to either terminate or modify the resource.
    rightsizingType :: Lude.Maybe RightsizingType,
    -- | Details for termination recommendations.
    terminateRecommendationDetail :: Lude.Maybe TerminateRecommendationDetail
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RightsizingRecommendation' with the minimum fields required to make a request.
--
-- * 'accountId' - The account that this recommendation is for.
-- * 'modifyRecommendationDetail' - Details for modification recommendations.
-- * 'currentInstance' - Context regarding the current instance.
-- * 'rightsizingType' - Recommendation to either terminate or modify the resource.
-- * 'terminateRecommendationDetail' - Details for termination recommendations.
mkRightsizingRecommendation ::
  RightsizingRecommendation
mkRightsizingRecommendation =
  RightsizingRecommendation'
    { accountId = Lude.Nothing,
      modifyRecommendationDetail = Lude.Nothing,
      currentInstance = Lude.Nothing,
      rightsizingType = Lude.Nothing,
      terminateRecommendationDetail = Lude.Nothing
    }

-- | The account that this recommendation is for.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrAccountId :: Lens.Lens' RightsizingRecommendation (Lude.Maybe Lude.Text)
rrAccountId = Lens.lens (accountId :: RightsizingRecommendation -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: RightsizingRecommendation)
{-# DEPRECATED rrAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Details for modification recommendations.
--
-- /Note:/ Consider using 'modifyRecommendationDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrModifyRecommendationDetail :: Lens.Lens' RightsizingRecommendation (Lude.Maybe ModifyRecommendationDetail)
rrModifyRecommendationDetail = Lens.lens (modifyRecommendationDetail :: RightsizingRecommendation -> Lude.Maybe ModifyRecommendationDetail) (\s a -> s {modifyRecommendationDetail = a} :: RightsizingRecommendation)
{-# DEPRECATED rrModifyRecommendationDetail "Use generic-lens or generic-optics with 'modifyRecommendationDetail' instead." #-}

-- | Context regarding the current instance.
--
-- /Note:/ Consider using 'currentInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCurrentInstance :: Lens.Lens' RightsizingRecommendation (Lude.Maybe CurrentInstance)
rrCurrentInstance = Lens.lens (currentInstance :: RightsizingRecommendation -> Lude.Maybe CurrentInstance) (\s a -> s {currentInstance = a} :: RightsizingRecommendation)
{-# DEPRECATED rrCurrentInstance "Use generic-lens or generic-optics with 'currentInstance' instead." #-}

-- | Recommendation to either terminate or modify the resource.
--
-- /Note:/ Consider using 'rightsizingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRightsizingType :: Lens.Lens' RightsizingRecommendation (Lude.Maybe RightsizingType)
rrRightsizingType = Lens.lens (rightsizingType :: RightsizingRecommendation -> Lude.Maybe RightsizingType) (\s a -> s {rightsizingType = a} :: RightsizingRecommendation)
{-# DEPRECATED rrRightsizingType "Use generic-lens or generic-optics with 'rightsizingType' instead." #-}

-- | Details for termination recommendations.
--
-- /Note:/ Consider using 'terminateRecommendationDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrTerminateRecommendationDetail :: Lens.Lens' RightsizingRecommendation (Lude.Maybe TerminateRecommendationDetail)
rrTerminateRecommendationDetail = Lens.lens (terminateRecommendationDetail :: RightsizingRecommendation -> Lude.Maybe TerminateRecommendationDetail) (\s a -> s {terminateRecommendationDetail = a} :: RightsizingRecommendation)
{-# DEPRECATED rrTerminateRecommendationDetail "Use generic-lens or generic-optics with 'terminateRecommendationDetail' instead." #-}

instance Lude.FromJSON RightsizingRecommendation where
  parseJSON =
    Lude.withObject
      "RightsizingRecommendation"
      ( \x ->
          RightsizingRecommendation'
            Lude.<$> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "ModifyRecommendationDetail")
            Lude.<*> (x Lude..:? "CurrentInstance")
            Lude.<*> (x Lude..:? "RightsizingType")
            Lude.<*> (x Lude..:? "TerminateRecommendationDetail")
      )
