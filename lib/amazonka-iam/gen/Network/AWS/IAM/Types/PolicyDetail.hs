{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyDetail
  ( PolicyDetail (..),

    -- * Smart constructor
    mkPolicyDetail,

    -- * Lenses
    pdPolicyDocument,
    pdPolicyName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an IAM policy, including the policy document.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkPolicyDetail' smart constructor.
data PolicyDetail = PolicyDetail'
  { policyDocument ::
      Lude.Maybe Lude.Text,
    policyName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyDetail' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The policy document.
-- * 'policyName' - The name of the policy.
mkPolicyDetail ::
  PolicyDetail
mkPolicyDetail =
  PolicyDetail'
    { policyDocument = Lude.Nothing,
      policyName = Lude.Nothing
    }

-- | The policy document.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyDocument :: Lens.Lens' PolicyDetail (Lude.Maybe Lude.Text)
pdPolicyDocument = Lens.lens (policyDocument :: PolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: PolicyDetail)
{-# DEPRECATED pdPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyName :: Lens.Lens' PolicyDetail (Lude.Maybe Lude.Text)
pdPolicyName = Lens.lens (policyName :: PolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: PolicyDetail)
{-# DEPRECATED pdPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.FromXML PolicyDetail where
  parseXML x =
    PolicyDetail'
      Lude.<$> (x Lude..@? "PolicyDocument") Lude.<*> (x Lude..@? "PolicyName")
