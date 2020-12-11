-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.PendingTaskCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.PendingTaskCount
  ( PendingTaskCount (..),

    -- * Smart constructor
    mkPendingTaskCount,

    -- * Lenses
    ptcTruncated,
    ptcCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the count of tasks in a task list.
--
-- /See:/ 'mkPendingTaskCount' smart constructor.
data PendingTaskCount = PendingTaskCount'
  { truncated ::
      Lude.Maybe Lude.Bool,
    count :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingTaskCount' with the minimum fields required to make a request.
--
-- * 'count' - The number of tasks in the task list.
-- * 'truncated' - If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
mkPendingTaskCount ::
  -- | 'count'
  Lude.Natural ->
  PendingTaskCount
mkPendingTaskCount pCount_ =
  PendingTaskCount' {truncated = Lude.Nothing, count = pCount_}

-- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcTruncated :: Lens.Lens' PendingTaskCount (Lude.Maybe Lude.Bool)
ptcTruncated = Lens.lens (truncated :: PendingTaskCount -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: PendingTaskCount)
{-# DEPRECATED ptcTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | The number of tasks in the task list.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptcCount :: Lens.Lens' PendingTaskCount Lude.Natural
ptcCount = Lens.lens (count :: PendingTaskCount -> Lude.Natural) (\s a -> s {count = a} :: PendingTaskCount)
{-# DEPRECATED ptcCount "Use generic-lens or generic-optics with 'count' instead." #-}

instance Lude.FromJSON PendingTaskCount where
  parseJSON =
    Lude.withObject
      "PendingTaskCount"
      ( \x ->
          PendingTaskCount'
            Lude.<$> (x Lude..:? "truncated") Lude.<*> (x Lude..: "count")
      )
