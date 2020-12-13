{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.EffectivePolicy
  ( EffectivePolicy (..),

    -- * Smart constructor
    mkEffectivePolicy,

    -- * Lenses
    epPolicyName,
    epPolicyDocument,
    epPolicyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The policy that has the effect on the authorization results.
--
-- /See:/ 'mkEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { -- | The policy name.
    policyName :: Lude.Maybe Lude.Text,
    -- | The IAM policy document.
    policyDocument :: Lude.Maybe Lude.Text,
    -- | The policy ARN.
    policyARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EffectivePolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The policy name.
-- * 'policyDocument' - The IAM policy document.
-- * 'policyARN' - The policy ARN.
mkEffectivePolicy ::
  EffectivePolicy
mkEffectivePolicy =
  EffectivePolicy'
    { policyName = Lude.Nothing,
      policyDocument = Lude.Nothing,
      policyARN = Lude.Nothing
    }

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyName :: Lens.Lens' EffectivePolicy (Lude.Maybe Lude.Text)
epPolicyName = Lens.lens (policyName :: EffectivePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: EffectivePolicy)
{-# DEPRECATED epPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The IAM policy document.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyDocument :: Lens.Lens' EffectivePolicy (Lude.Maybe Lude.Text)
epPolicyDocument = Lens.lens (policyDocument :: EffectivePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: EffectivePolicy)
{-# DEPRECATED epPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyARN :: Lens.Lens' EffectivePolicy (Lude.Maybe Lude.Text)
epPolicyARN = Lens.lens (policyARN :: EffectivePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: EffectivePolicy)
{-# DEPRECATED epPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.FromJSON EffectivePolicy where
  parseJSON =
    Lude.withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            Lude.<$> (x Lude..:? "policyName")
            Lude.<*> (x Lude..:? "policyDocument")
            Lude.<*> (x Lude..:? "policyArn")
      )
