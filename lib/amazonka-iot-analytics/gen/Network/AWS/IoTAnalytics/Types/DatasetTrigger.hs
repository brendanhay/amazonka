{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetTrigger
  ( DatasetTrigger (..),

    -- * Smart constructor
    mkDatasetTrigger,

    -- * Lenses
    dtDataset,
    dtSchedule,
  )
where

import Network.AWS.IoTAnalytics.Types.Schedule
import Network.AWS.IoTAnalytics.Types.TriggeringDataset
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @DatasetTrigger@ that specifies when the data set is automatically updated.
--
-- /See:/ 'mkDatasetTrigger' smart constructor.
data DatasetTrigger = DatasetTrigger'
  { -- | The data set whose content creation triggers the creation of this data set's contents.
    dataset :: Lude.Maybe TriggeringDataset,
    -- | The Schedule when the trigger is initiated.
    schedule :: Lude.Maybe Schedule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetTrigger' with the minimum fields required to make a request.
--
-- * 'dataset' - The data set whose content creation triggers the creation of this data set's contents.
-- * 'schedule' - The Schedule when the trigger is initiated.
mkDatasetTrigger ::
  DatasetTrigger
mkDatasetTrigger =
  DatasetTrigger' {dataset = Lude.Nothing, schedule = Lude.Nothing}

-- | The data set whose content creation triggers the creation of this data set's contents.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDataset :: Lens.Lens' DatasetTrigger (Lude.Maybe TriggeringDataset)
dtDataset = Lens.lens (dataset :: DatasetTrigger -> Lude.Maybe TriggeringDataset) (\s a -> s {dataset = a} :: DatasetTrigger)
{-# DEPRECATED dtDataset "Use generic-lens or generic-optics with 'dataset' instead." #-}

-- | The Schedule when the trigger is initiated.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtSchedule :: Lens.Lens' DatasetTrigger (Lude.Maybe Schedule)
dtSchedule = Lens.lens (schedule :: DatasetTrigger -> Lude.Maybe Schedule) (\s a -> s {schedule = a} :: DatasetTrigger)
{-# DEPRECATED dtSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

instance Lude.FromJSON DatasetTrigger where
  parseJSON =
    Lude.withObject
      "DatasetTrigger"
      ( \x ->
          DatasetTrigger'
            Lude.<$> (x Lude..:? "dataset") Lude.<*> (x Lude..:? "schedule")
      )

instance Lude.ToJSON DatasetTrigger where
  toJSON DatasetTrigger' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("dataset" Lude..=) Lude.<$> dataset,
            ("schedule" Lude..=) Lude.<$> schedule
          ]
      )
