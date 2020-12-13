{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Timing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Timing
  ( Timing (..),

    -- * Smart constructor
    mkTiming,

    -- * Lenses
    tSubmitTimeMillis,
    tFinishTimeMillis,
    tStartTimeMillis,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the timing of a job.
--
-- /See:/ 'mkTiming' smart constructor.
data Timing = Timing'
  { -- | The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
    submitTimeMillis :: Lude.Maybe Lude.Integer,
    -- | The time the job finished transcoding, in epoch milliseconds.
    finishTimeMillis :: Lude.Maybe Lude.Integer,
    -- | The time the job began transcoding, in epoch milliseconds.
    startTimeMillis :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Timing' with the minimum fields required to make a request.
--
-- * 'submitTimeMillis' - The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
-- * 'finishTimeMillis' - The time the job finished transcoding, in epoch milliseconds.
-- * 'startTimeMillis' - The time the job began transcoding, in epoch milliseconds.
mkTiming ::
  Timing
mkTiming =
  Timing'
    { submitTimeMillis = Lude.Nothing,
      finishTimeMillis = Lude.Nothing,
      startTimeMillis = Lude.Nothing
    }

-- | The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
--
-- /Note:/ Consider using 'submitTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSubmitTimeMillis :: Lens.Lens' Timing (Lude.Maybe Lude.Integer)
tSubmitTimeMillis = Lens.lens (submitTimeMillis :: Timing -> Lude.Maybe Lude.Integer) (\s a -> s {submitTimeMillis = a} :: Timing)
{-# DEPRECATED tSubmitTimeMillis "Use generic-lens or generic-optics with 'submitTimeMillis' instead." #-}

-- | The time the job finished transcoding, in epoch milliseconds.
--
-- /Note:/ Consider using 'finishTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFinishTimeMillis :: Lens.Lens' Timing (Lude.Maybe Lude.Integer)
tFinishTimeMillis = Lens.lens (finishTimeMillis :: Timing -> Lude.Maybe Lude.Integer) (\s a -> s {finishTimeMillis = a} :: Timing)
{-# DEPRECATED tFinishTimeMillis "Use generic-lens or generic-optics with 'finishTimeMillis' instead." #-}

-- | The time the job began transcoding, in epoch milliseconds.
--
-- /Note:/ Consider using 'startTimeMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStartTimeMillis :: Lens.Lens' Timing (Lude.Maybe Lude.Integer)
tStartTimeMillis = Lens.lens (startTimeMillis :: Timing -> Lude.Maybe Lude.Integer) (\s a -> s {startTimeMillis = a} :: Timing)
{-# DEPRECATED tStartTimeMillis "Use generic-lens or generic-optics with 'startTimeMillis' instead." #-}

instance Lude.FromJSON Timing where
  parseJSON =
    Lude.withObject
      "Timing"
      ( \x ->
          Timing'
            Lude.<$> (x Lude..:? "SubmitTimeMillis")
            Lude.<*> (x Lude..:? "FinishTimeMillis")
            Lude.<*> (x Lude..:? "StartTimeMillis")
      )
