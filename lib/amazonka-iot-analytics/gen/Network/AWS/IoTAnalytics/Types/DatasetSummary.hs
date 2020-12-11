-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetSummary
  ( DatasetSummary (..),

    -- * Smart constructor
    mkDatasetSummary,

    -- * Lenses
    dssCreationTime,
    dssStatus,
    dssActions,
    dssTriggers,
    dssDatasetName,
    dssLastUpdateTime,
  )
where

import Network.AWS.IoTAnalytics.Types.DatasetActionSummary
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of information about a data set.
--
-- /See:/ 'mkDatasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe DatasetStatus,
    actions :: Lude.Maybe (Lude.NonEmpty DatasetActionSummary),
    triggers :: Lude.Maybe [DatasetTrigger],
    datasetName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DatasetSummary' with the minimum fields required to make a request.
--
-- * 'actions' - A list of @DataActionSummary@ objects.
-- * 'creationTime' - The time the data set was created.
-- * 'datasetName' - The name of the data set.
-- * 'lastUpdateTime' - The last time the data set was updated.
-- * 'status' - The status of the data set.
-- * 'triggers' - A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects
mkDatasetSummary ::
  DatasetSummary
mkDatasetSummary =
  DatasetSummary'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      actions = Lude.Nothing,
      triggers = Lude.Nothing,
      datasetName = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | The time the data set was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssCreationTime :: Lens.Lens' DatasetSummary (Lude.Maybe Lude.Timestamp)
dssCreationTime = Lens.lens (creationTime :: DatasetSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DatasetSummary)
{-# DEPRECATED dssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the data set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStatus :: Lens.Lens' DatasetSummary (Lude.Maybe DatasetStatus)
dssStatus = Lens.lens (status :: DatasetSummary -> Lude.Maybe DatasetStatus) (\s a -> s {status = a} :: DatasetSummary)
{-# DEPRECATED dssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of @DataActionSummary@ objects.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssActions :: Lens.Lens' DatasetSummary (Lude.Maybe (Lude.NonEmpty DatasetActionSummary))
dssActions = Lens.lens (actions :: DatasetSummary -> Lude.Maybe (Lude.NonEmpty DatasetActionSummary)) (\s a -> s {actions = a} :: DatasetSummary)
{-# DEPRECATED dssActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssTriggers :: Lens.Lens' DatasetSummary (Lude.Maybe [DatasetTrigger])
dssTriggers = Lens.lens (triggers :: DatasetSummary -> Lude.Maybe [DatasetTrigger]) (\s a -> s {triggers = a} :: DatasetSummary)
{-# DEPRECATED dssTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | The name of the data set.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssDatasetName :: Lens.Lens' DatasetSummary (Lude.Maybe Lude.Text)
dssDatasetName = Lens.lens (datasetName :: DatasetSummary -> Lude.Maybe Lude.Text) (\s a -> s {datasetName = a} :: DatasetSummary)
{-# DEPRECATED dssDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The last time the data set was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssLastUpdateTime :: Lens.Lens' DatasetSummary (Lude.Maybe Lude.Timestamp)
dssLastUpdateTime = Lens.lens (lastUpdateTime :: DatasetSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: DatasetSummary)
{-# DEPRECATED dssLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON DatasetSummary where
  parseJSON =
    Lude.withObject
      "DatasetSummary"
      ( \x ->
          DatasetSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "actions")
            Lude.<*> (x Lude..:? "triggers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "datasetName")
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
