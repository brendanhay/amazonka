-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.AttemptDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.AttemptDetail
  ( AttemptDetail (..),

    -- * Smart constructor
    mkAttemptDetail,

    -- * Lenses
    adStoppedAt,
    adStartedAt,
    adContainer,
    adStatusReason,
  )
where

import Network.AWS.Batch.Types.AttemptContainerDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a job attempt.
--
-- /See:/ 'mkAttemptDetail' smart constructor.
data AttemptDetail = AttemptDetail'
  { stoppedAt ::
      Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Integer,
    container :: Lude.Maybe AttemptContainerDetail,
    statusReason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttemptDetail' with the minimum fields required to make a request.
--
-- * 'container' - Details about the container in this job attempt.
-- * 'startedAt' - The Unix timestamp (in milliseconds) for when the attempt was started (when the attempt transitioned from the @STARTING@ state to the @RUNNING@ state).
-- * 'statusReason' - A short, human-readable string to provide additional details about the current status of the job attempt.
-- * 'stoppedAt' - The Unix timestamp (in milliseconds) for when the attempt was stopped (when the attempt transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
mkAttemptDetail ::
  AttemptDetail
mkAttemptDetail =
  AttemptDetail'
    { stoppedAt = Lude.Nothing,
      startedAt = Lude.Nothing,
      container = Lude.Nothing,
      statusReason = Lude.Nothing
    }

-- | The Unix timestamp (in milliseconds) for when the attempt was stopped (when the attempt transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStoppedAt :: Lens.Lens' AttemptDetail (Lude.Maybe Lude.Integer)
adStoppedAt = Lens.lens (stoppedAt :: AttemptDetail -> Lude.Maybe Lude.Integer) (\s a -> s {stoppedAt = a} :: AttemptDetail)
{-# DEPRECATED adStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the attempt was started (when the attempt transitioned from the @STARTING@ state to the @RUNNING@ state).
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStartedAt :: Lens.Lens' AttemptDetail (Lude.Maybe Lude.Integer)
adStartedAt = Lens.lens (startedAt :: AttemptDetail -> Lude.Maybe Lude.Integer) (\s a -> s {startedAt = a} :: AttemptDetail)
{-# DEPRECATED adStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | Details about the container in this job attempt.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adContainer :: Lens.Lens' AttemptDetail (Lude.Maybe AttemptContainerDetail)
adContainer = Lens.lens (container :: AttemptDetail -> Lude.Maybe AttemptContainerDetail) (\s a -> s {container = a} :: AttemptDetail)
{-# DEPRECATED adContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job attempt.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStatusReason :: Lens.Lens' AttemptDetail (Lude.Maybe Lude.Text)
adStatusReason = Lens.lens (statusReason :: AttemptDetail -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: AttemptDetail)
{-# DEPRECATED adStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Lude.FromJSON AttemptDetail where
  parseJSON =
    Lude.withObject
      "AttemptDetail"
      ( \x ->
          AttemptDetail'
            Lude.<$> (x Lude..:? "stoppedAt")
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..:? "container")
            Lude.<*> (x Lude..:? "statusReason")
      )
