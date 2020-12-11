-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicySummary
  ( OriginRequestPolicySummary (..),

    -- * Smart constructor
    mkOriginRequestPolicySummary,

    -- * Lenses
    orpsType,
    orpsOriginRequestPolicy,
  )
where

import Network.AWS.CloudFront.Types.OriginRequestPolicy
import Network.AWS.CloudFront.Types.OriginRequestPolicyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains an origin request policy.
--
-- /See:/ 'mkOriginRequestPolicySummary' smart constructor.
data OriginRequestPolicySummary = OriginRequestPolicySummary'
  { type' ::
      OriginRequestPolicyType,
    originRequestPolicy ::
      OriginRequestPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginRequestPolicySummary' with the minimum fields required to make a request.
--
-- * 'originRequestPolicy' - The origin request policy.
-- * 'type'' - The type of origin request policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
mkOriginRequestPolicySummary ::
  -- | 'type''
  OriginRequestPolicyType ->
  -- | 'originRequestPolicy'
  OriginRequestPolicy ->
  OriginRequestPolicySummary
mkOriginRequestPolicySummary pType_ pOriginRequestPolicy_ =
  OriginRequestPolicySummary'
    { type' = pType_,
      originRequestPolicy = pOriginRequestPolicy_
    }

-- | The type of origin request policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpsType :: Lens.Lens' OriginRequestPolicySummary OriginRequestPolicyType
orpsType = Lens.lens (type' :: OriginRequestPolicySummary -> OriginRequestPolicyType) (\s a -> s {type' = a} :: OriginRequestPolicySummary)
{-# DEPRECATED orpsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpsOriginRequestPolicy :: Lens.Lens' OriginRequestPolicySummary OriginRequestPolicy
orpsOriginRequestPolicy = Lens.lens (originRequestPolicy :: OriginRequestPolicySummary -> OriginRequestPolicy) (\s a -> s {originRequestPolicy = a} :: OriginRequestPolicySummary)
{-# DEPRECATED orpsOriginRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead." #-}

instance Lude.FromXML OriginRequestPolicySummary where
  parseXML x =
    OriginRequestPolicySummary'
      Lude.<$> (x Lude..@ "Type") Lude.<*> (x Lude..@ "OriginRequestPolicy")
