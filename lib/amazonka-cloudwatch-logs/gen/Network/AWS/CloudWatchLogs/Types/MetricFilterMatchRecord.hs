{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
  ( MetricFilterMatchRecord (..),

    -- * Smart constructor
    mkMetricFilterMatchRecord,

    -- * Lenses
    mfmrExtractedValues,
    mfmrEventNumber,
    mfmrEventMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a matched event.
--
-- /See:/ 'mkMetricFilterMatchRecord' smart constructor.
data MetricFilterMatchRecord = MetricFilterMatchRecord'
  { -- | The values extracted from the event data by the filter.
    extractedValues :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The event number.
    eventNumber :: Lude.Maybe Lude.Integer,
    -- | The raw event data.
    eventMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricFilterMatchRecord' with the minimum fields required to make a request.
--
-- * 'extractedValues' - The values extracted from the event data by the filter.
-- * 'eventNumber' - The event number.
-- * 'eventMessage' - The raw event data.
mkMetricFilterMatchRecord ::
  MetricFilterMatchRecord
mkMetricFilterMatchRecord =
  MetricFilterMatchRecord'
    { extractedValues = Lude.Nothing,
      eventNumber = Lude.Nothing,
      eventMessage = Lude.Nothing
    }

-- | The values extracted from the event data by the filter.
--
-- /Note:/ Consider using 'extractedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfmrExtractedValues :: Lens.Lens' MetricFilterMatchRecord (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
mfmrExtractedValues = Lens.lens (extractedValues :: MetricFilterMatchRecord -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {extractedValues = a} :: MetricFilterMatchRecord)
{-# DEPRECATED mfmrExtractedValues "Use generic-lens or generic-optics with 'extractedValues' instead." #-}

-- | The event number.
--
-- /Note:/ Consider using 'eventNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfmrEventNumber :: Lens.Lens' MetricFilterMatchRecord (Lude.Maybe Lude.Integer)
mfmrEventNumber = Lens.lens (eventNumber :: MetricFilterMatchRecord -> Lude.Maybe Lude.Integer) (\s a -> s {eventNumber = a} :: MetricFilterMatchRecord)
{-# DEPRECATED mfmrEventNumber "Use generic-lens or generic-optics with 'eventNumber' instead." #-}

-- | The raw event data.
--
-- /Note:/ Consider using 'eventMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfmrEventMessage :: Lens.Lens' MetricFilterMatchRecord (Lude.Maybe Lude.Text)
mfmrEventMessage = Lens.lens (eventMessage :: MetricFilterMatchRecord -> Lude.Maybe Lude.Text) (\s a -> s {eventMessage = a} :: MetricFilterMatchRecord)
{-# DEPRECATED mfmrEventMessage "Use generic-lens or generic-optics with 'eventMessage' instead." #-}

instance Lude.FromJSON MetricFilterMatchRecord where
  parseJSON =
    Lude.withObject
      "MetricFilterMatchRecord"
      ( \x ->
          MetricFilterMatchRecord'
            Lude.<$> (x Lude..:? "extractedValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "eventNumber")
            Lude.<*> (x Lude..:? "eventMessage")
      )
