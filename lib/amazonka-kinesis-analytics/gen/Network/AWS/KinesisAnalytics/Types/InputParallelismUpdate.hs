{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
  ( InputParallelismUpdate (..),

    -- * Smart constructor
    mkInputParallelismUpdate,

    -- * Lenses
    ipuCountUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides updates to the parallelism count.
--
-- /See:/ 'mkInputParallelismUpdate' smart constructor.
newtype InputParallelismUpdate = InputParallelismUpdate'
  { -- | Number of in-application streams to create for the specified streaming source.
    countUpdate :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputParallelismUpdate' with the minimum fields required to make a request.
--
-- * 'countUpdate' - Number of in-application streams to create for the specified streaming source.
mkInputParallelismUpdate ::
  InputParallelismUpdate
mkInputParallelismUpdate =
  InputParallelismUpdate' {countUpdate = Lude.Nothing}

-- | Number of in-application streams to create for the specified streaming source.
--
-- /Note:/ Consider using 'countUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuCountUpdate :: Lens.Lens' InputParallelismUpdate (Lude.Maybe Lude.Natural)
ipuCountUpdate = Lens.lens (countUpdate :: InputParallelismUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {countUpdate = a} :: InputParallelismUpdate)
{-# DEPRECATED ipuCountUpdate "Use generic-lens or generic-optics with 'countUpdate' instead." #-}

instance Lude.ToJSON InputParallelismUpdate where
  toJSON InputParallelismUpdate' {..} =
    Lude.object
      (Lude.catMaybes [("CountUpdate" Lude..=) Lude.<$> countUpdate])
