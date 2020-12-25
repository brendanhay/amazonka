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
    dsfActions,
    dsfCreationTime,
    dsfDatasetName,
    dsfLastUpdateTime,
    dsfStatus,
    dsfTriggers,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.DatasetActionSummary as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetName as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetStatus as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetTrigger as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of information about a data set.
--
-- /See:/ 'mkDatasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { -- | A list of @DataActionSummary@ objects.
    actions :: Core.Maybe (Core.NonEmpty Types.DatasetActionSummary),
    -- | The time the data set was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the data set.
    datasetName :: Core.Maybe Types.DatasetName,
    -- | The last time the data set was updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the data set.
    status :: Core.Maybe Types.DatasetStatus,
    -- | A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects
    triggers :: Core.Maybe [Types.DatasetTrigger]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DatasetSummary' value with any optional fields omitted.
mkDatasetSummary ::
  DatasetSummary
mkDatasetSummary =
  DatasetSummary'
    { actions = Core.Nothing,
      creationTime = Core.Nothing,
      datasetName = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      status = Core.Nothing,
      triggers = Core.Nothing
    }

-- | A list of @DataActionSummary@ objects.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfActions :: Lens.Lens' DatasetSummary (Core.Maybe (Core.NonEmpty Types.DatasetActionSummary))
dsfActions = Lens.field @"actions"
{-# DEPRECATED dsfActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The time the data set was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfCreationTime :: Lens.Lens' DatasetSummary (Core.Maybe Core.NominalDiffTime)
dsfCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dsfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the data set.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfDatasetName :: Lens.Lens' DatasetSummary (Core.Maybe Types.DatasetName)
dsfDatasetName = Lens.field @"datasetName"
{-# DEPRECATED dsfDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | The last time the data set was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfLastUpdateTime :: Lens.Lens' DatasetSummary (Core.Maybe Core.NominalDiffTime)
dsfLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED dsfLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The status of the data set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfStatus :: Lens.Lens' DatasetSummary (Core.Maybe Types.DatasetStatus)
dsfStatus = Lens.field @"status"
{-# DEPRECATED dsfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five @DataSetTrigger@ objects
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfTriggers :: Lens.Lens' DatasetSummary (Core.Maybe [Types.DatasetTrigger])
dsfTriggers = Lens.field @"triggers"
{-# DEPRECATED dsfTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

instance Core.FromJSON DatasetSummary where
  parseJSON =
    Core.withObject "DatasetSummary" Core.$
      \x ->
        DatasetSummary'
          Core.<$> (x Core..:? "actions")
          Core.<*> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "datasetName")
          Core.<*> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "triggers")
