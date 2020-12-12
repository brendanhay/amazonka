{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewPolicy
  ( ReviewPolicy (..),

    -- * Smart constructor
    mkReviewPolicy,

    -- * Lenses
    rpParameters,
    rpPolicyName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.PolicyParameter
import qualified Network.AWS.Prelude as Lude

-- | HIT Review Policy data structures represent HIT review policies, which you specify when you create a HIT.
--
-- /See:/ 'mkReviewPolicy' smart constructor.
data ReviewPolicy = ReviewPolicy'
  { parameters ::
      Lude.Maybe [PolicyParameter],
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReviewPolicy' with the minimum fields required to make a request.
--
-- * 'parameters' - Name of the parameter from the Review policy.
-- * 'policyName' - Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
mkReviewPolicy ::
  -- | 'policyName'
  Lude.Text ->
  ReviewPolicy
mkReviewPolicy pPolicyName_ =
  ReviewPolicy'
    { parameters = Lude.Nothing,
      policyName = pPolicyName_
    }

-- | Name of the parameter from the Review policy.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpParameters :: Lens.Lens' ReviewPolicy (Lude.Maybe [PolicyParameter])
rpParameters = Lens.lens (parameters :: ReviewPolicy -> Lude.Maybe [PolicyParameter]) (\s a -> s {parameters = a} :: ReviewPolicy)
{-# DEPRECATED rpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPolicyName :: Lens.Lens' ReviewPolicy Lude.Text
rpPolicyName = Lens.lens (policyName :: ReviewPolicy -> Lude.Text) (\s a -> s {policyName = a} :: ReviewPolicy)
{-# DEPRECATED rpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.FromJSON ReviewPolicy where
  parseJSON =
    Lude.withObject
      "ReviewPolicy"
      ( \x ->
          ReviewPolicy'
            Lude.<$> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "PolicyName")
      )

instance Lude.ToJSON ReviewPolicy where
  toJSON ReviewPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("PolicyName" Lude..= policyName)
          ]
      )
