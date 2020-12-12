{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyVersionIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyVersionIdentifier
  ( PolicyVersionIdentifier (..),

    -- * Smart constructor
    mkPolicyVersionIdentifier,

    -- * Lenses
    pviPolicyName,
    pviPolicyVersionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the version of the policy associated with the resource.
--
-- /See:/ 'mkPolicyVersionIdentifier' smart constructor.
data PolicyVersionIdentifier = PolicyVersionIdentifier'
  { policyName ::
      Lude.Maybe Lude.Text,
    policyVersionId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyVersionIdentifier' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy.
-- * 'policyVersionId' - The ID of the version of the policy associated with the resource.
mkPolicyVersionIdentifier ::
  PolicyVersionIdentifier
mkPolicyVersionIdentifier =
  PolicyVersionIdentifier'
    { policyName = Lude.Nothing,
      policyVersionId = Lude.Nothing
    }

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pviPolicyName :: Lens.Lens' PolicyVersionIdentifier (Lude.Maybe Lude.Text)
pviPolicyName = Lens.lens (policyName :: PolicyVersionIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: PolicyVersionIdentifier)
{-# DEPRECATED pviPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The ID of the version of the policy associated with the resource.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pviPolicyVersionId :: Lens.Lens' PolicyVersionIdentifier (Lude.Maybe Lude.Text)
pviPolicyVersionId = Lens.lens (policyVersionId :: PolicyVersionIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {policyVersionId = a} :: PolicyVersionIdentifier)
{-# DEPRECATED pviPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Lude.FromJSON PolicyVersionIdentifier where
  parseJSON =
    Lude.withObject
      "PolicyVersionIdentifier"
      ( \x ->
          PolicyVersionIdentifier'
            Lude.<$> (x Lude..:? "policyName") Lude.<*> (x Lude..:? "policyVersionId")
      )

instance Lude.ToJSON PolicyVersionIdentifier where
  toJSON PolicyVersionIdentifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("policyName" Lude..=) Lude.<$> policyName,
            ("policyVersionId" Lude..=) Lude.<$> policyVersionId
          ]
      )
