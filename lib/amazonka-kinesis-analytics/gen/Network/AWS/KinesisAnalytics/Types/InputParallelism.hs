{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputParallelism
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputParallelism
  ( InputParallelism (..),

    -- * Smart constructor
    mkInputParallelism,

    -- * Lenses
    ipCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the number of in-application streams to create for a given streaming source. For information about parallelism, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /See:/ 'mkInputParallelism' smart constructor.
newtype InputParallelism = InputParallelism'
  { count ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputParallelism' with the minimum fields required to make a request.
--
-- * 'count' - Number of in-application streams to create. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
mkInputParallelism ::
  InputParallelism
mkInputParallelism = InputParallelism' {count = Lude.Nothing}

-- | Number of in-application streams to create. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipCount :: Lens.Lens' InputParallelism (Lude.Maybe Lude.Natural)
ipCount = Lens.lens (count :: InputParallelism -> Lude.Maybe Lude.Natural) (\s a -> s {count = a} :: InputParallelism)
{-# DEPRECATED ipCount "Use generic-lens or generic-optics with 'count' instead." #-}

instance Lude.FromJSON InputParallelism where
  parseJSON =
    Lude.withObject
      "InputParallelism"
      (\x -> InputParallelism' Lude.<$> (x Lude..:? "Count"))

instance Lude.ToJSON InputParallelism where
  toJSON InputParallelism' {..} =
    Lude.object (Lude.catMaybes [("Count" Lude..=) Lude.<$> count])
