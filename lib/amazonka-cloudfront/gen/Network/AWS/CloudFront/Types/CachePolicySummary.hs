{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicySummary
  ( CachePolicySummary (..),

    -- * Smart constructor
    mkCachePolicySummary,

    -- * Lenses
    cpsType,
    cpsCachePolicy,
  )
where

import qualified Network.AWS.CloudFront.Types.CachePolicy as Types
import qualified Network.AWS.CloudFront.Types.CachePolicyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a cache policy.
--
-- /See:/ 'mkCachePolicySummary' smart constructor.
data CachePolicySummary = CachePolicySummary'
  { -- | The type of cache policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
    type' :: Types.CachePolicyType,
    -- | The cache policy.
    cachePolicy :: Types.CachePolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CachePolicySummary' value with any optional fields omitted.
mkCachePolicySummary ::
  -- | 'type\''
  Types.CachePolicyType ->
  -- | 'cachePolicy'
  Types.CachePolicy ->
  CachePolicySummary
mkCachePolicySummary type' cachePolicy =
  CachePolicySummary' {type', cachePolicy}

-- | The type of cache policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsType :: Lens.Lens' CachePolicySummary Types.CachePolicyType
cpsType = Lens.field @"type'"
{-# DEPRECATED cpsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsCachePolicy :: Lens.Lens' CachePolicySummary Types.CachePolicy
cpsCachePolicy = Lens.field @"cachePolicy"
{-# DEPRECATED cpsCachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead." #-}

instance Core.FromXML CachePolicySummary where
  parseXML x =
    CachePolicySummary'
      Core.<$> (x Core..@ "Type") Core.<*> (x Core..@ "CachePolicy")
