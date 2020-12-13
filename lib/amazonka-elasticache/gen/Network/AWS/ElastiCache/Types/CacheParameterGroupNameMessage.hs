{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
  ( CacheParameterGroupNameMessage (..),

    -- * Smart constructor
    mkCacheParameterGroupNameMessage,

    -- * Lenses
    cpgnmCacheParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of one of the following operations:
--
--
--     * @ModifyCacheParameterGroup@
--
--
--     * @ResetCacheParameterGroup@
--
--
--
-- /See:/ 'mkCacheParameterGroupNameMessage' smart constructor.
newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage'
  { -- | The name of the cache parameter group.
    cacheParameterGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupName' - The name of the cache parameter group.
mkCacheParameterGroupNameMessage ::
  CacheParameterGroupNameMessage
mkCacheParameterGroupNameMessage =
  CacheParameterGroupNameMessage'
    { cacheParameterGroupName =
        Lude.Nothing
    }

-- | The name of the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgnmCacheParameterGroupName :: Lens.Lens' CacheParameterGroupNameMessage (Lude.Maybe Lude.Text)
cpgnmCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: CacheParameterGroupNameMessage -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: CacheParameterGroupNameMessage)
{-# DEPRECATED cpgnmCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

instance Lude.FromXML CacheParameterGroupNameMessage where
  parseXML x =
    CacheParameterGroupNameMessage'
      Lude.<$> (x Lude..@? "CacheParameterGroupName")
