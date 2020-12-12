{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.PipelineSummary
  ( PipelineSummary (..),

    -- * Smart constructor
    mkPipelineSummary,

    -- * Lenses
    psCreationTime,
    psPipelineName,
    psReprocessingSummaries,
    psLastUpdateTime,
  )
where

import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of information about a pipeline.
--
-- /See:/ 'mkPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    pipelineName :: Lude.Maybe Lude.Text,
    reprocessingSummaries :: Lude.Maybe [ReprocessingSummary],
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the pipeline was created.
-- * 'lastUpdateTime' - When the pipeline was last updated.
-- * 'pipelineName' - The name of the pipeline.
-- * 'reprocessingSummaries' - A summary of information about the pipeline reprocessing.
mkPipelineSummary ::
  PipelineSummary
mkPipelineSummary =
  PipelineSummary'
    { creationTime = Lude.Nothing,
      pipelineName = Lude.Nothing,
      reprocessingSummaries = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | When the pipeline was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCreationTime :: Lens.Lens' PipelineSummary (Lude.Maybe Lude.Timestamp)
psCreationTime = Lens.lens (creationTime :: PipelineSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: PipelineSummary)
{-# DEPRECATED psCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPipelineName :: Lens.Lens' PipelineSummary (Lude.Maybe Lude.Text)
psPipelineName = Lens.lens (pipelineName :: PipelineSummary -> Lude.Maybe Lude.Text) (\s a -> s {pipelineName = a} :: PipelineSummary)
{-# DEPRECATED psPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | A summary of information about the pipeline reprocessing.
--
-- /Note:/ Consider using 'reprocessingSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psReprocessingSummaries :: Lens.Lens' PipelineSummary (Lude.Maybe [ReprocessingSummary])
psReprocessingSummaries = Lens.lens (reprocessingSummaries :: PipelineSummary -> Lude.Maybe [ReprocessingSummary]) (\s a -> s {reprocessingSummaries = a} :: PipelineSummary)
{-# DEPRECATED psReprocessingSummaries "Use generic-lens or generic-optics with 'reprocessingSummaries' instead." #-}

-- | When the pipeline was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psLastUpdateTime :: Lens.Lens' PipelineSummary (Lude.Maybe Lude.Timestamp)
psLastUpdateTime = Lens.lens (lastUpdateTime :: PipelineSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: PipelineSummary)
{-# DEPRECATED psLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON PipelineSummary where
  parseJSON =
    Lude.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "pipelineName")
            Lude.<*> (x Lude..:? "reprocessingSummaries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
