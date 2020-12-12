{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Limits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.Limits
  ( Limits (..),

    -- * Smart constructor
    mkLimits,

    -- * Lenses
    lMaximumReplicationCount,
    lMaximumPartitionCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkLimits' smart constructor.
data Limits = Limits'
  { maximumReplicationCount :: Lude.Natural,
    maximumPartitionCount :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Limits' with the minimum fields required to make a request.
--
-- * 'maximumPartitionCount' - Undocumented field.
-- * 'maximumReplicationCount' - Undocumented field.
mkLimits ::
  -- | 'maximumReplicationCount'
  Lude.Natural ->
  -- | 'maximumPartitionCount'
  Lude.Natural ->
  Limits
mkLimits pMaximumReplicationCount_ pMaximumPartitionCount_ =
  Limits'
    { maximumReplicationCount = pMaximumReplicationCount_,
      maximumPartitionCount = pMaximumPartitionCount_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'maximumReplicationCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaximumReplicationCount :: Lens.Lens' Limits Lude.Natural
lMaximumReplicationCount = Lens.lens (maximumReplicationCount :: Limits -> Lude.Natural) (\s a -> s {maximumReplicationCount = a} :: Limits)
{-# DEPRECATED lMaximumReplicationCount "Use generic-lens or generic-optics with 'maximumReplicationCount' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maximumPartitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaximumPartitionCount :: Lens.Lens' Limits Lude.Natural
lMaximumPartitionCount = Lens.lens (maximumPartitionCount :: Limits -> Lude.Natural) (\s a -> s {maximumPartitionCount = a} :: Limits)
{-# DEPRECATED lMaximumPartitionCount "Use generic-lens or generic-optics with 'maximumPartitionCount' instead." #-}

instance Lude.FromXML Limits where
  parseXML x =
    Limits'
      Lude.<$> (x Lude..@ "MaximumReplicationCount")
      Lude.<*> (x Lude..@ "MaximumPartitionCount")
