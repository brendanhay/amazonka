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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.ContainerDatasetAction
import Amazonka.IoTAnalytics.Types.SqlQueryDatasetAction
import qualified Amazonka.Prelude as Prelude

-- | A @DatasetAction@ object that specifies how dataset contents are
-- automatically created.
--
-- /See:/ 'newDatasetAction' smart constructor.
data DatasetAction = DatasetAction'
  { -- | The name of the dataset action by which dataset contents are
    -- automatically created.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | Information that allows the system to run a containerized application to
    -- create the dataset contents. The application must be in a Docker
    -- container along with any required support libraries.
    containerAction :: Prelude.Maybe ContainerDatasetAction,
    -- | An @SqlQueryDatasetAction@ object that uses an SQL query to
    -- automatically create dataset contents.
    queryAction :: Prelude.Maybe SqlQueryDatasetAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'datasetAction_actionName' - The name of the dataset action by which dataset contents are
-- automatically created.
--
-- 'containerAction', 'datasetAction_containerAction' - Information that allows the system to run a containerized application to
-- create the dataset contents. The application must be in a Docker
-- container along with any required support libraries.
--
-- 'queryAction', 'datasetAction_queryAction' - An @SqlQueryDatasetAction@ object that uses an SQL query to
-- automatically create dataset contents.
newDatasetAction ::
  DatasetAction
newDatasetAction =
  DatasetAction'
    { actionName = Prelude.Nothing,
      containerAction = Prelude.Nothing,
      queryAction = Prelude.Nothing
    }

-- | The name of the dataset action by which dataset contents are
-- automatically created.
datasetAction_actionName :: Lens.Lens' DatasetAction (Prelude.Maybe Prelude.Text)
datasetAction_actionName = Lens.lens (\DatasetAction' {actionName} -> actionName) (\s@DatasetAction' {} a -> s {actionName = a} :: DatasetAction)

-- | Information that allows the system to run a containerized application to
-- create the dataset contents. The application must be in a Docker
-- container along with any required support libraries.
datasetAction_containerAction :: Lens.Lens' DatasetAction (Prelude.Maybe ContainerDatasetAction)
datasetAction_containerAction = Lens.lens (\DatasetAction' {containerAction} -> containerAction) (\s@DatasetAction' {} a -> s {containerAction = a} :: DatasetAction)

-- | An @SqlQueryDatasetAction@ object that uses an SQL query to
-- automatically create dataset contents.
datasetAction_queryAction :: Lens.Lens' DatasetAction (Prelude.Maybe SqlQueryDatasetAction)
datasetAction_queryAction = Lens.lens (\DatasetAction' {queryAction} -> queryAction) (\s@DatasetAction' {} a -> s {queryAction = a} :: DatasetAction)

instance Data.FromJSON DatasetAction where
  parseJSON =
    Data.withObject
      "DatasetAction"
      ( \x ->
          DatasetAction'
            Prelude.<$> (x Data..:? "actionName")
            Prelude.<*> (x Data..:? "containerAction")
            Prelude.<*> (x Data..:? "queryAction")
      )

instance Prelude.Hashable DatasetAction where
  hashWithSalt _salt DatasetAction' {..} =
    _salt `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` containerAction
      `Prelude.hashWithSalt` queryAction

instance Prelude.NFData DatasetAction where
  rnf DatasetAction' {..} =
    Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf containerAction
      `Prelude.seq` Prelude.rnf queryAction

instance Data.ToJSON DatasetAction where
  toJSON DatasetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("actionName" Data..=) Prelude.<$> actionName,
            ("containerAction" Data..=)
              Prelude.<$> containerAction,
            ("queryAction" Data..=) Prelude.<$> queryAction
          ]
      )
