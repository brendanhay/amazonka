{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyDescription
  ( PolicyDescription (..),

    -- * Smart constructor
    mkPolicyDescription,

    -- * Lenses
    pdPolicyName,
    pdPolicyAttributeDescriptions,
    pdPolicyTypeName,
  )
where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.PolicyAttributeDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy.
--
-- /See:/ 'mkPolicyDescription' smart constructor.
data PolicyDescription = PolicyDescription'
  { -- | The name of the policy.
    policyName :: Lude.Maybe Lude.Text,
    -- | The policy attributes.
    policyAttributeDescriptions :: Lude.Maybe [PolicyAttributeDescription],
    -- | The name of the policy type.
    policyTypeName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyDescription' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy.
-- * 'policyAttributeDescriptions' - The policy attributes.
-- * 'policyTypeName' - The name of the policy type.
mkPolicyDescription ::
  PolicyDescription
mkPolicyDescription =
  PolicyDescription'
    { policyName = Lude.Nothing,
      policyAttributeDescriptions = Lude.Nothing,
      policyTypeName = Lude.Nothing
    }

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyName :: Lens.Lens' PolicyDescription (Lude.Maybe Lude.Text)
pdPolicyName = Lens.lens (policyName :: PolicyDescription -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: PolicyDescription)
{-# DEPRECATED pdPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy attributes.
--
-- /Note:/ Consider using 'policyAttributeDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyAttributeDescriptions :: Lens.Lens' PolicyDescription (Lude.Maybe [PolicyAttributeDescription])
pdPolicyAttributeDescriptions = Lens.lens (policyAttributeDescriptions :: PolicyDescription -> Lude.Maybe [PolicyAttributeDescription]) (\s a -> s {policyAttributeDescriptions = a} :: PolicyDescription)
{-# DEPRECATED pdPolicyAttributeDescriptions "Use generic-lens or generic-optics with 'policyAttributeDescriptions' instead." #-}

-- | The name of the policy type.
--
-- /Note:/ Consider using 'policyTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyTypeName :: Lens.Lens' PolicyDescription (Lude.Maybe Lude.Text)
pdPolicyTypeName = Lens.lens (policyTypeName :: PolicyDescription -> Lude.Maybe Lude.Text) (\s a -> s {policyTypeName = a} :: PolicyDescription)
{-# DEPRECATED pdPolicyTypeName "Use generic-lens or generic-optics with 'policyTypeName' instead." #-}

instance Lude.FromXML PolicyDescription where
  parseXML x =
    PolicyDescription'
      Lude.<$> (x Lude..@? "PolicyName")
      Lude.<*> ( x Lude..@? "PolicyAttributeDescriptions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "PolicyTypeName")
