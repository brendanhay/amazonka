-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Policy
  ( Policy (..),

    -- * Smart constructor
    mkPolicy,

    -- * Lenses
    pContent,
    pPolicySummary,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicySummary
import qualified Network.AWS.Prelude as Lude

-- | Contains rules to be applied to the affected accounts. Policies can be attached directly to accounts, or to roots and OUs to affect all accounts in those hierarchies.
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { content :: Lude.Maybe Lude.Text,
    policySummary :: Lude.Maybe PolicySummary
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- * 'content' - The text content of the policy.
-- * 'policySummary' - A structure that contains additional details about the policy.
mkPolicy ::
  Policy
mkPolicy =
  Policy' {content = Lude.Nothing, policySummary = Lude.Nothing}

-- | The text content of the policy.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pContent :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pContent = Lens.lens (content :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: Policy)
{-# DEPRECATED pContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | A structure that contains additional details about the policy.
--
-- /Note:/ Consider using 'policySummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicySummary :: Lens.Lens' Policy (Lude.Maybe PolicySummary)
pPolicySummary = Lens.lens (policySummary :: Policy -> Lude.Maybe PolicySummary) (\s a -> s {policySummary = a} :: Policy)
{-# DEPRECATED pPolicySummary "Use generic-lens or generic-optics with 'policySummary' instead." #-}

instance Lude.FromJSON Policy where
  parseJSON =
    Lude.withObject
      "Policy"
      ( \x ->
          Policy'
            Lude.<$> (x Lude..:? "Content") Lude.<*> (x Lude..:? "PolicySummary")
      )
