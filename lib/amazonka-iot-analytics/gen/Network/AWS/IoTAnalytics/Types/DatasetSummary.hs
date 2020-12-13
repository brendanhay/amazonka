{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dsfCreationTime,
    dsfStatus,
    dsfActions,
    dsfTriggers,
    dsfDatasetName,
    dsfLastUpdateTime,
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
  { -- | The time the data set was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the data set.
    status :: Lude.Maybe DatasetStatus,
    -- | A list of @DataActionSummary@ objects.
    actions :: Lude.Maybe (Lude.NonEmpty DatasetActionSummary),
    -- | A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects
    triggers :: Lude.Maybe [DatasetTrigger],
    -- | The name of the data set.
    datasetName :: Lude.Maybe Lude.Text,
    -- | The last time the data set was updated.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the data set was created.
-- * 'status' - The status of the data set.
-- * 'actions' - A list of @DataActionSummary@ objects.
-- * 'triggers' - A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects
-- * 'datasetName' - The name of the data set.
-- * 'lastUpdateTime' - The last time the data set was updated.
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
dsfCreationTime :: Lens.Lens' DatasetSummary (Lude.Maybe Lude.Timestamp)
dsfCreationTime = Lens.lens (creationTime :: DatasetSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DatasetSummary)
{-# DEPRECATED dsfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the data set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfStatus :: Lens.Lens' DatasetSummary (Lude.Maybe DatasetStatus)
dsfStatus = Lens.lens (status :: DatasetSummary -> Lude.Maybe DatasetStatus) (\s a -> s {status = a} :: DatasetSummary)
{-# DEPRECATED dsfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of @DataActionSummary@ objects.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfActions :: Lens.Lens' DatasetSummary (Lude.Maybe (Lude.NonEmpty DatasetActionSummary))
dsfActions = Lens.lens (actions :: DatasetSummary -> Lude.Maybe (Lude.NonEmpty DatasetActionSummary)) (\s a -> s {actions = a} :: DatasetSummary)
{-# DEPRECATED dsfActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfTriggers :: Lens.Lens' DatasetSummary (Lude.Maybe [DatasetTrigger])
dsfTriggers = Lens.lens (triggers :: DatasetSummary -> Lude.Maybe [DatasetTrigger]) (\s a -> s {triggers = a} :: DatasetSummary)
{-# DEPRECATED dsfTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | The name of the data set.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfDatasetName :: Lens.Lens' DatasetSummary (Lude.Maybe Lude.Text)
dsfDatasetName = Lens.lens (datasetName :: DatasetSummary -> Lude.Maybe Lude.Text) (\s a -> s {datasetName = a} :: DatasetSummary)
{-# DEPRECATED dsfDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The last time the data set was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfLastUpdateTime :: Lens.Lens' DatasetSummary (Lude.Maybe Lude.Timestamp)
dsfLastUpdateTime = Lens.lens (lastUpdateTime :: DatasetSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: DatasetSummary)
{-# DEPRECATED dsfLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

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
