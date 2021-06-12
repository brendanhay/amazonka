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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetActionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetActionSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.DatasetActionType
import qualified Network.AWS.Lens as Lens

-- | Information about the action that automatically creates the dataset\'s
-- contents.
--
-- /See:/ 'newDatasetActionSummary' smart constructor.
data DatasetActionSummary = DatasetActionSummary'
  { -- | The name of the action that automatically creates the dataset\'s
    -- contents.
    actionName :: Core.Maybe Core.Text,
    -- | The type of action by which the dataset\'s contents are automatically
    -- created.
    actionType :: Core.Maybe DatasetActionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DatasetActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'datasetActionSummary_actionName' - The name of the action that automatically creates the dataset\'s
-- contents.
--
-- 'actionType', 'datasetActionSummary_actionType' - The type of action by which the dataset\'s contents are automatically
-- created.
newDatasetActionSummary ::
  DatasetActionSummary
newDatasetActionSummary =
  DatasetActionSummary'
    { actionName = Core.Nothing,
      actionType = Core.Nothing
    }

-- | The name of the action that automatically creates the dataset\'s
-- contents.
datasetActionSummary_actionName :: Lens.Lens' DatasetActionSummary (Core.Maybe Core.Text)
datasetActionSummary_actionName = Lens.lens (\DatasetActionSummary' {actionName} -> actionName) (\s@DatasetActionSummary' {} a -> s {actionName = a} :: DatasetActionSummary)

-- | The type of action by which the dataset\'s contents are automatically
-- created.
datasetActionSummary_actionType :: Lens.Lens' DatasetActionSummary (Core.Maybe DatasetActionType)
datasetActionSummary_actionType = Lens.lens (\DatasetActionSummary' {actionType} -> actionType) (\s@DatasetActionSummary' {} a -> s {actionType = a} :: DatasetActionSummary)

instance Core.FromJSON DatasetActionSummary where
  parseJSON =
    Core.withObject
      "DatasetActionSummary"
      ( \x ->
          DatasetActionSummary'
            Core.<$> (x Core..:? "actionName")
            Core.<*> (x Core..:? "actionType")
      )

instance Core.Hashable DatasetActionSummary

instance Core.NFData DatasetActionSummary
