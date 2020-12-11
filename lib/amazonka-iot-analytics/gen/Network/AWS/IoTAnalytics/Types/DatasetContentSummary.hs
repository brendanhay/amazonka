-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentSummary
  ( DatasetContentSummary (..),

    -- * Smart constructor
    mkDatasetContentSummary,

    -- * Lenses
    dcsCreationTime,
    dcsStatus,
    dcsScheduleTime,
    dcsCompletionTime,
    dcsVersion,
  )
where

import Network.AWS.IoTAnalytics.Types.DatasetContentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about dataset contents.
--
-- /See:/ 'mkDatasetContentSummary' smart constructor.
data DatasetContentSummary = DatasetContentSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe DatasetContentStatus,
    scheduleTime :: Lude.Maybe Lude.Timestamp,
    completionTime :: Lude.Maybe Lude.Timestamp,
    version :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetContentSummary' with the minimum fields required to make a request.
--
-- * 'completionTime' - The time the dataset content status was updated to SUCCEEDED or FAILED.
-- * 'creationTime' - The actual time the creation of the dataset contents was started.
-- * 'scheduleTime' - The time the creation of the dataset contents was scheduled to start.
-- * 'status' - The status of the data set contents.
-- * 'version' - The version of the dataset contents.
mkDatasetContentSummary ::
  DatasetContentSummary
mkDatasetContentSummary =
  DatasetContentSummary'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      scheduleTime = Lude.Nothing,
      completionTime = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The actual time the creation of the dataset contents was started.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCreationTime :: Lens.Lens' DatasetContentSummary (Lude.Maybe Lude.Timestamp)
dcsCreationTime = Lens.lens (creationTime :: DatasetContentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DatasetContentSummary)
{-# DEPRECATED dcsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the data set contents.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStatus :: Lens.Lens' DatasetContentSummary (Lude.Maybe DatasetContentStatus)
dcsStatus = Lens.lens (status :: DatasetContentSummary -> Lude.Maybe DatasetContentStatus) (\s a -> s {status = a} :: DatasetContentSummary)
{-# DEPRECATED dcsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time the creation of the dataset contents was scheduled to start.
--
-- /Note:/ Consider using 'scheduleTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsScheduleTime :: Lens.Lens' DatasetContentSummary (Lude.Maybe Lude.Timestamp)
dcsScheduleTime = Lens.lens (scheduleTime :: DatasetContentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {scheduleTime = a} :: DatasetContentSummary)
{-# DEPRECATED dcsScheduleTime "Use generic-lens or generic-optics with 'scheduleTime' instead." #-}

-- | The time the dataset content status was updated to SUCCEEDED or FAILED.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCompletionTime :: Lens.Lens' DatasetContentSummary (Lude.Maybe Lude.Timestamp)
dcsCompletionTime = Lens.lens (completionTime :: DatasetContentSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionTime = a} :: DatasetContentSummary)
{-# DEPRECATED dcsCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | The version of the dataset contents.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsVersion :: Lens.Lens' DatasetContentSummary (Lude.Maybe Lude.Text)
dcsVersion = Lens.lens (version :: DatasetContentSummary -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DatasetContentSummary)
{-# DEPRECATED dcsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON DatasetContentSummary where
  parseJSON =
    Lude.withObject
      "DatasetContentSummary"
      ( \x ->
          DatasetContentSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "scheduleTime")
            Lude.<*> (x Lude..:? "completionTime")
            Lude.<*> (x Lude..:? "version")
      )
