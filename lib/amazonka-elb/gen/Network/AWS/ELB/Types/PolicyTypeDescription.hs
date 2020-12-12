{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyTypeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyTypeDescription
  ( PolicyTypeDescription (..),

    -- * Smart constructor
    mkPolicyTypeDescription,

    -- * Lenses
    ptdPolicyTypeName,
    ptdDescription,
    ptdPolicyAttributeTypeDescriptions,
  )
where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.PolicyAttributeTypeDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy type.
--
-- /See:/ 'mkPolicyTypeDescription' smart constructor.
data PolicyTypeDescription = PolicyTypeDescription'
  { policyTypeName ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    policyAttributeTypeDescriptions ::
      Lude.Maybe [PolicyAttributeTypeDescription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyTypeDescription' with the minimum fields required to make a request.
--
-- * 'description' - A description of the policy type.
-- * 'policyAttributeTypeDescriptions' - The description of the policy attributes associated with the policies defined by Elastic Load Balancing.
-- * 'policyTypeName' - The name of the policy type.
mkPolicyTypeDescription ::
  PolicyTypeDescription
mkPolicyTypeDescription =
  PolicyTypeDescription'
    { policyTypeName = Lude.Nothing,
      description = Lude.Nothing,
      policyAttributeTypeDescriptions = Lude.Nothing
    }

-- | The name of the policy type.
--
-- /Note:/ Consider using 'policyTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdPolicyTypeName :: Lens.Lens' PolicyTypeDescription (Lude.Maybe Lude.Text)
ptdPolicyTypeName = Lens.lens (policyTypeName :: PolicyTypeDescription -> Lude.Maybe Lude.Text) (\s a -> s {policyTypeName = a} :: PolicyTypeDescription)
{-# DEPRECATED ptdPolicyTypeName "Use generic-lens or generic-optics with 'policyTypeName' instead." #-}

-- | A description of the policy type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdDescription :: Lens.Lens' PolicyTypeDescription (Lude.Maybe Lude.Text)
ptdDescription = Lens.lens (description :: PolicyTypeDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PolicyTypeDescription)
{-# DEPRECATED ptdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The description of the policy attributes associated with the policies defined by Elastic Load Balancing.
--
-- /Note:/ Consider using 'policyAttributeTypeDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdPolicyAttributeTypeDescriptions :: Lens.Lens' PolicyTypeDescription (Lude.Maybe [PolicyAttributeTypeDescription])
ptdPolicyAttributeTypeDescriptions = Lens.lens (policyAttributeTypeDescriptions :: PolicyTypeDescription -> Lude.Maybe [PolicyAttributeTypeDescription]) (\s a -> s {policyAttributeTypeDescriptions = a} :: PolicyTypeDescription)
{-# DEPRECATED ptdPolicyAttributeTypeDescriptions "Use generic-lens or generic-optics with 'policyAttributeTypeDescriptions' instead." #-}

instance Lude.FromXML PolicyTypeDescription where
  parseXML x =
    PolicyTypeDescription'
      Lude.<$> (x Lude..@? "PolicyTypeName")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "PolicyAttributeTypeDescriptions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
