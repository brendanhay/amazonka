{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.OriginRequestPolicySummary
  ( OriginRequestPolicySummary (..)
  -- * Smart constructor
  , mkOriginRequestPolicySummary
  -- * Lenses
  , orpsType
  , orpsOriginRequestPolicy
  ) where

import qualified Network.AWS.CloudFront.Types.OriginRequestPolicy as Types
import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains an origin request policy.
--
-- /See:/ 'mkOriginRequestPolicySummary' smart constructor.
data OriginRequestPolicySummary = OriginRequestPolicySummary'
  { type' :: Types.OriginRequestPolicyType
    -- ^ The type of origin request policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
  , originRequestPolicy :: Types.OriginRequestPolicy
    -- ^ The origin request policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OriginRequestPolicySummary' value with any optional fields omitted.
mkOriginRequestPolicySummary
    :: Types.OriginRequestPolicyType -- ^ 'type\''
    -> Types.OriginRequestPolicy -- ^ 'originRequestPolicy'
    -> OriginRequestPolicySummary
mkOriginRequestPolicySummary type' originRequestPolicy
  = OriginRequestPolicySummary'{type', originRequestPolicy}

-- | The type of origin request policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpsType :: Lens.Lens' OriginRequestPolicySummary Types.OriginRequestPolicyType
orpsType = Lens.field @"type'"
{-# INLINEABLE orpsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpsOriginRequestPolicy :: Lens.Lens' OriginRequestPolicySummary Types.OriginRequestPolicy
orpsOriginRequestPolicy = Lens.field @"originRequestPolicy"
{-# INLINEABLE orpsOriginRequestPolicy #-}
{-# DEPRECATED originRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead"  #-}

instance Core.FromXML OriginRequestPolicySummary where
        parseXML x
          = OriginRequestPolicySummary' Core.<$>
              (x Core..@ "Type") Core.<*> x Core..@ "OriginRequestPolicy"
