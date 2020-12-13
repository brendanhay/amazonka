{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobTimeout
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobTimeout
  ( JobTimeout (..),

    -- * Smart constructor
    mkJobTimeout,

    -- * Lenses
    jtAttemptDurationSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a job timeout configuration.
--
-- /See:/ 'mkJobTimeout' smart constructor.
newtype JobTimeout = JobTimeout'
  { -- | The time duration in seconds (measured from the job attempt's @startedAt@ timestamp) after which AWS Batch terminates your jobs if they have not finished.
    attemptDurationSeconds :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobTimeout' with the minimum fields required to make a request.
--
-- * 'attemptDurationSeconds' - The time duration in seconds (measured from the job attempt's @startedAt@ timestamp) after which AWS Batch terminates your jobs if they have not finished.
mkJobTimeout ::
  JobTimeout
mkJobTimeout = JobTimeout' {attemptDurationSeconds = Lude.Nothing}

-- | The time duration in seconds (measured from the job attempt's @startedAt@ timestamp) after which AWS Batch terminates your jobs if they have not finished.
--
-- /Note:/ Consider using 'attemptDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtAttemptDurationSeconds :: Lens.Lens' JobTimeout (Lude.Maybe Lude.Int)
jtAttemptDurationSeconds = Lens.lens (attemptDurationSeconds :: JobTimeout -> Lude.Maybe Lude.Int) (\s a -> s {attemptDurationSeconds = a} :: JobTimeout)
{-# DEPRECATED jtAttemptDurationSeconds "Use generic-lens or generic-optics with 'attemptDurationSeconds' instead." #-}

instance Lude.FromJSON JobTimeout where
  parseJSON =
    Lude.withObject
      "JobTimeout"
      (\x -> JobTimeout' Lude.<$> (x Lude..:? "attemptDurationSeconds"))

instance Lude.ToJSON JobTimeout where
  toJSON JobTimeout' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("attemptDurationSeconds" Lude..=)
              Lude.<$> attemptDurationSeconds
          ]
      )
