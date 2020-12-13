{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
  ( TrustedAdvisorCategorySpecificSummary (..),

    -- * Smart constructor
    mkTrustedAdvisorCategorySpecificSummary,

    -- * Lenses
    tacssCostOptimizing,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary

-- | The container for summary information that relates to the category of the Trusted Advisor check.
--
-- /See:/ 'mkTrustedAdvisorCategorySpecificSummary' smart constructor.
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary'
  { -- | The summary information about cost savings for a Trusted Advisor check that is in the Cost Optimizing category.
    costOptimizing :: Lude.Maybe TrustedAdvisorCostOptimizingSummary
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrustedAdvisorCategorySpecificSummary' with the minimum fields required to make a request.
--
-- * 'costOptimizing' - The summary information about cost savings for a Trusted Advisor check that is in the Cost Optimizing category.
mkTrustedAdvisorCategorySpecificSummary ::
  TrustedAdvisorCategorySpecificSummary
mkTrustedAdvisorCategorySpecificSummary =
  TrustedAdvisorCategorySpecificSummary'
    { costOptimizing =
        Lude.Nothing
    }

-- | The summary information about cost savings for a Trusted Advisor check that is in the Cost Optimizing category.
--
-- /Note:/ Consider using 'costOptimizing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacssCostOptimizing :: Lens.Lens' TrustedAdvisorCategorySpecificSummary (Lude.Maybe TrustedAdvisorCostOptimizingSummary)
tacssCostOptimizing = Lens.lens (costOptimizing :: TrustedAdvisorCategorySpecificSummary -> Lude.Maybe TrustedAdvisorCostOptimizingSummary) (\s a -> s {costOptimizing = a} :: TrustedAdvisorCategorySpecificSummary)
{-# DEPRECATED tacssCostOptimizing "Use generic-lens or generic-optics with 'costOptimizing' instead." #-}

instance Lude.FromJSON TrustedAdvisorCategorySpecificSummary where
  parseJSON =
    Lude.withObject
      "TrustedAdvisorCategorySpecificSummary"
      ( \x ->
          TrustedAdvisorCategorySpecificSummary'
            Lude.<$> (x Lude..:? "costOptimizing")
      )
