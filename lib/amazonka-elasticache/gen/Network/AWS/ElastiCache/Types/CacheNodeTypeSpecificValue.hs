-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
  ( CacheNodeTypeSpecificValue (..),

    -- * Smart constructor
    mkCacheNodeTypeSpecificValue,

    -- * Lenses
    cntsvCacheNodeType,
    cntsvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A value that applies only to a certain cache node type.
--
-- /See:/ 'mkCacheNodeTypeSpecificValue' smart constructor.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue'
  { cacheNodeType ::
      Lude.Maybe Lude.Text,
    value :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheNodeTypeSpecificValue' with the minimum fields required to make a request.
--
-- * 'cacheNodeType' - The cache node type for which this value applies.
-- * 'value' - The value for the cache node type.
mkCacheNodeTypeSpecificValue ::
  CacheNodeTypeSpecificValue
mkCacheNodeTypeSpecificValue =
  CacheNodeTypeSpecificValue'
    { cacheNodeType = Lude.Nothing,
      value = Lude.Nothing
    }

-- | The cache node type for which this value applies.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntsvCacheNodeType :: Lens.Lens' CacheNodeTypeSpecificValue (Lude.Maybe Lude.Text)
cntsvCacheNodeType = Lens.lens (cacheNodeType :: CacheNodeTypeSpecificValue -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: CacheNodeTypeSpecificValue)
{-# DEPRECATED cntsvCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The value for the cache node type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntsvValue :: Lens.Lens' CacheNodeTypeSpecificValue (Lude.Maybe Lude.Text)
cntsvValue = Lens.lens (value :: CacheNodeTypeSpecificValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: CacheNodeTypeSpecificValue)
{-# DEPRECATED cntsvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML CacheNodeTypeSpecificValue where
  parseXML x =
    CacheNodeTypeSpecificValue'
      Lude.<$> (x Lude..@? "CacheNodeType") Lude.<*> (x Lude..@? "Value")
