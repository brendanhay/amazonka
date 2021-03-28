{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetAction
  ( DatasetAction (..)
  -- * Smart constructor
  , mkDatasetAction
  -- * Lenses
  , daActionName
  , daContainerAction
  , daQueryAction
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ContainerDatasetAction as Types
import qualified Network.AWS.IoTAnalytics.Types.DatasetActionName as Types
import qualified Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A @DatasetAction@ object that specifies how data set contents are automatically created.
--
-- /See:/ 'mkDatasetAction' smart constructor.
data DatasetAction = DatasetAction'
  { actionName :: Core.Maybe Types.DatasetActionName
    -- ^ The name of the data set action by which data set contents are automatically created.
  , containerAction :: Core.Maybe Types.ContainerDatasetAction
    -- ^ Information that allows the system to run a containerized application to create the dataset contents. The application must be in a Docker container along with any required support libraries.
  , queryAction :: Core.Maybe Types.SqlQueryDatasetAction
    -- ^ An @SqlQueryDatasetAction@ object that uses an SQL query to automatically create data set contents.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetAction' value with any optional fields omitted.
mkDatasetAction
    :: DatasetAction
mkDatasetAction
  = DatasetAction'{actionName = Core.Nothing,
                   containerAction = Core.Nothing, queryAction = Core.Nothing}

-- | The name of the data set action by which data set contents are automatically created.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActionName :: Lens.Lens' DatasetAction (Core.Maybe Types.DatasetActionName)
daActionName = Lens.field @"actionName"
{-# INLINEABLE daActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | Information that allows the system to run a containerized application to create the dataset contents. The application must be in a Docker container along with any required support libraries.
--
-- /Note:/ Consider using 'containerAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daContainerAction :: Lens.Lens' DatasetAction (Core.Maybe Types.ContainerDatasetAction)
daContainerAction = Lens.field @"containerAction"
{-# INLINEABLE daContainerAction #-}
{-# DEPRECATED containerAction "Use generic-lens or generic-optics with 'containerAction' instead"  #-}

-- | An @SqlQueryDatasetAction@ object that uses an SQL query to automatically create data set contents.
--
-- /Note:/ Consider using 'queryAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daQueryAction :: Lens.Lens' DatasetAction (Core.Maybe Types.SqlQueryDatasetAction)
daQueryAction = Lens.field @"queryAction"
{-# INLINEABLE daQueryAction #-}
{-# DEPRECATED queryAction "Use generic-lens or generic-optics with 'queryAction' instead"  #-}

instance Core.FromJSON DatasetAction where
        toJSON DatasetAction{..}
          = Core.object
              (Core.catMaybes
                 [("actionName" Core..=) Core.<$> actionName,
                  ("containerAction" Core..=) Core.<$> containerAction,
                  ("queryAction" Core..=) Core.<$> queryAction])

instance Core.FromJSON DatasetAction where
        parseJSON
          = Core.withObject "DatasetAction" Core.$
              \ x ->
                DatasetAction' Core.<$>
                  (x Core..:? "actionName") Core.<*> x Core..:? "containerAction"
                    Core.<*> x Core..:? "queryAction"
