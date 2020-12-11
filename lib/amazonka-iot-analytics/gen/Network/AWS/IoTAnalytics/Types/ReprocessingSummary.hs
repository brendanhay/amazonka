-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingSummary
  ( ReprocessingSummary (..),

    -- * Smart constructor
    mkReprocessingSummary,

    -- * Lenses
    rsCreationTime,
    rsStatus,
    rsId,
  )
where

import Network.AWS.IoTAnalytics.Types.ReprocessingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about pipeline reprocessing.
--
-- /See:/ 'mkReprocessingSummary' smart constructor.
data ReprocessingSummary = ReprocessingSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe ReprocessingStatus,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReprocessingSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the pipeline reprocessing was created.
-- * 'id' - The @reprocessingId@ returned by @StartPipelineReprocessing@ .
-- * 'status' - The status of the pipeline reprocessing.
mkReprocessingSummary ::
  ReprocessingSummary
mkReprocessingSummary =
  ReprocessingSummary'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The time the pipeline reprocessing was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCreationTime :: Lens.Lens' ReprocessingSummary (Lude.Maybe Lude.Timestamp)
rsCreationTime = Lens.lens (creationTime :: ReprocessingSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: ReprocessingSummary)
{-# DEPRECATED rsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the pipeline reprocessing.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsStatus :: Lens.Lens' ReprocessingSummary (Lude.Maybe ReprocessingStatus)
rsStatus = Lens.lens (status :: ReprocessingSummary -> Lude.Maybe ReprocessingStatus) (\s a -> s {status = a} :: ReprocessingSummary)
{-# DEPRECATED rsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @reprocessingId@ returned by @StartPipelineReprocessing@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsId :: Lens.Lens' ReprocessingSummary (Lude.Maybe Lude.Text)
rsId = Lens.lens (id :: ReprocessingSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ReprocessingSummary)
{-# DEPRECATED rsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON ReprocessingSummary where
  parseJSON =
    Lude.withObject
      "ReprocessingSummary"
      ( \x ->
          ReprocessingSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "id")
      )
