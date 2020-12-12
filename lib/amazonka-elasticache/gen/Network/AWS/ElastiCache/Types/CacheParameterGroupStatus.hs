{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
  ( CacheParameterGroupStatus (..),

    -- * Smart constructor
    mkCacheParameterGroupStatus,

    -- * Lenses
    cpgsCacheParameterGroupName,
    cpgsCacheNodeIdsToReboot,
    cpgsParameterApplyStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of the cache parameter group.
--
-- /See:/ 'mkCacheParameterGroupStatus' smart constructor.
data CacheParameterGroupStatus = CacheParameterGroupStatus'
  { cacheParameterGroupName ::
      Lude.Maybe Lude.Text,
    cacheNodeIdsToReboot ::
      Lude.Maybe [Lude.Text],
    parameterApplyStatus ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheParameterGroupStatus' with the minimum fields required to make a request.
--
-- * 'cacheNodeIdsToReboot' - A list of the cache node IDs which need to be rebooted for parameter changes to be applied. A node ID is a numeric identifier (0001, 0002, etc.).
-- * 'cacheParameterGroupName' - The name of the cache parameter group.
-- * 'parameterApplyStatus' - The status of parameter updates.
mkCacheParameterGroupStatus ::
  CacheParameterGroupStatus
mkCacheParameterGroupStatus =
  CacheParameterGroupStatus'
    { cacheParameterGroupName =
        Lude.Nothing,
      cacheNodeIdsToReboot = Lude.Nothing,
      parameterApplyStatus = Lude.Nothing
    }

-- | The name of the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsCacheParameterGroupName :: Lens.Lens' CacheParameterGroupStatus (Lude.Maybe Lude.Text)
cpgsCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: CacheParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: CacheParameterGroupStatus)
{-# DEPRECATED cpgsCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | A list of the cache node IDs which need to be rebooted for parameter changes to be applied. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeIdsToReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsCacheNodeIdsToReboot :: Lens.Lens' CacheParameterGroupStatus (Lude.Maybe [Lude.Text])
cpgsCacheNodeIdsToReboot = Lens.lens (cacheNodeIdsToReboot :: CacheParameterGroupStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheNodeIdsToReboot = a} :: CacheParameterGroupStatus)
{-# DEPRECATED cpgsCacheNodeIdsToReboot "Use generic-lens or generic-optics with 'cacheNodeIdsToReboot' instead." #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsParameterApplyStatus :: Lens.Lens' CacheParameterGroupStatus (Lude.Maybe Lude.Text)
cpgsParameterApplyStatus = Lens.lens (parameterApplyStatus :: CacheParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterApplyStatus = a} :: CacheParameterGroupStatus)
{-# DEPRECATED cpgsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

instance Lude.FromXML CacheParameterGroupStatus where
  parseXML x =
    CacheParameterGroupStatus'
      Lude.<$> (x Lude..@? "CacheParameterGroupName")
      Lude.<*> ( x Lude..@? "CacheNodeIdsToReboot" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheNodeId")
               )
      Lude.<*> (x Lude..@? "ParameterApplyStatus")
