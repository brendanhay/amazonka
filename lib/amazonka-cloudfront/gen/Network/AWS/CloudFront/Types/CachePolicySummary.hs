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

import Network.AWS.CloudFront.Types.CachePolicy
import Network.AWS.CloudFront.Types.CachePolicyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a cache policy.
--
-- /See:/ 'mkCachePolicySummary' smart constructor.
data CachePolicySummary = CachePolicySummary'
  { type' ::
      CachePolicyType,
    cachePolicy :: CachePolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachePolicySummary' with the minimum fields required to make a request.
--
-- * 'cachePolicy' - The cache policy.
-- * 'type'' - The type of cache policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
mkCachePolicySummary ::
  -- | 'type''
  CachePolicyType ->
  -- | 'cachePolicy'
  CachePolicy ->
  CachePolicySummary
mkCachePolicySummary pType_ pCachePolicy_ =
  CachePolicySummary' {type' = pType_, cachePolicy = pCachePolicy_}

-- | The type of cache policy, either @managed@ (created by AWS) or @custom@ (created in this AWS account).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsType :: Lens.Lens' CachePolicySummary CachePolicyType
cpsType = Lens.lens (type' :: CachePolicySummary -> CachePolicyType) (\s a -> s {type' = a} :: CachePolicySummary)
{-# DEPRECATED cpsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsCachePolicy :: Lens.Lens' CachePolicySummary CachePolicy
cpsCachePolicy = Lens.lens (cachePolicy :: CachePolicySummary -> CachePolicy) (\s a -> s {cachePolicy = a} :: CachePolicySummary)
{-# DEPRECATED cpsCachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead." #-}

instance Lude.FromXML CachePolicySummary where
  parseXML x =
    CachePolicySummary'
      Lude.<$> (x Lude..@ "Type") Lude.<*> (x Lude..@ "CachePolicy")
