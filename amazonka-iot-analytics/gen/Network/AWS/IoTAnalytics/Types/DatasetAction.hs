{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetAction where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
import Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
import qualified Network.AWS.Lens as Lens

-- | A @DatasetAction@ object that specifies how data set contents are
-- automatically created.
--
-- /See:/ 'newDatasetAction' smart constructor.
data DatasetAction = DatasetAction'
  { -- | The name of the data set action by which data set contents are
    -- automatically created.
    actionName :: Core.Maybe Core.Text,
    -- | An @SqlQueryDatasetAction@ object that uses an SQL query to
    -- automatically create data set contents.
    queryAction :: Core.Maybe SqlQueryDatasetAction,
    -- | Information that allows the system to run a containerized application to
    -- create the dataset contents. The application must be in a Docker
    -- container along with any required support libraries.
    containerAction :: Core.Maybe ContainerDatasetAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DatasetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'datasetAction_actionName' - The name of the data set action by which data set contents are
-- automatically created.
--
-- 'queryAction', 'datasetAction_queryAction' - An @SqlQueryDatasetAction@ object that uses an SQL query to
-- automatically create data set contents.
--
-- 'containerAction', 'datasetAction_containerAction' - Information that allows the system to run a containerized application to
-- create the dataset contents. The application must be in a Docker
-- container along with any required support libraries.
newDatasetAction ::
  DatasetAction
newDatasetAction =
  DatasetAction'
    { actionName = Core.Nothing,
      queryAction = Core.Nothing,
      containerAction = Core.Nothing
    }

-- | The name of the data set action by which data set contents are
-- automatically created.
datasetAction_actionName :: Lens.Lens' DatasetAction (Core.Maybe Core.Text)
datasetAction_actionName = Lens.lens (\DatasetAction' {actionName} -> actionName) (\s@DatasetAction' {} a -> s {actionName = a} :: DatasetAction)

-- | An @SqlQueryDatasetAction@ object that uses an SQL query to
-- automatically create data set contents.
datasetAction_queryAction :: Lens.Lens' DatasetAction (Core.Maybe SqlQueryDatasetAction)
datasetAction_queryAction = Lens.lens (\DatasetAction' {queryAction} -> queryAction) (\s@DatasetAction' {} a -> s {queryAction = a} :: DatasetAction)

-- | Information that allows the system to run a containerized application to
-- create the dataset contents. The application must be in a Docker
-- container along with any required support libraries.
datasetAction_containerAction :: Lens.Lens' DatasetAction (Core.Maybe ContainerDatasetAction)
datasetAction_containerAction = Lens.lens (\DatasetAction' {containerAction} -> containerAction) (\s@DatasetAction' {} a -> s {containerAction = a} :: DatasetAction)

instance Core.FromJSON DatasetAction where
  parseJSON =
    Core.withObject
      "DatasetAction"
      ( \x ->
          DatasetAction'
            Core.<$> (x Core..:? "actionName")
            Core.<*> (x Core..:? "queryAction")
            Core.<*> (x Core..:? "containerAction")
      )

instance Core.Hashable DatasetAction

instance Core.NFData DatasetAction

instance Core.ToJSON DatasetAction where
  toJSON DatasetAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("actionName" Core..=) Core.<$> actionName,
            ("queryAction" Core..=) Core.<$> queryAction,
            ("containerAction" Core..=)
              Core.<$> containerAction
          ]
      )
