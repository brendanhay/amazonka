-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Policy
  ( Policy (..),

    -- * Smart constructor
    mkPolicy,

    -- * Lenses
    pPolicyName,
    pPolicyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an AWS IoT policy.
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { policyName :: Lude.Maybe Lude.Text,
    policyARN :: Lude.Maybe Lude.Text
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
-- * 'policyARN' - The policy ARN.
-- * 'policyName' - The policy name.
mkPolicy ::
  Policy
mkPolicy =
  Policy' {policyName = Lude.Nothing, policyARN = Lude.Nothing}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyName :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pPolicyName = Lens.lens (policyName :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: Policy)
{-# DEPRECATED pPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyARN :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pPolicyARN = Lens.lens (policyARN :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: Policy)
{-# DEPRECATED pPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.FromJSON Policy where
  parseJSON =
    Lude.withObject
      "Policy"
      ( \x ->
          Policy'
            Lude.<$> (x Lude..:? "policyName") Lude.<*> (x Lude..:? "policyArn")
      )
