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
-- Module      : Amazonka.Transfer.Types.ListedWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedWorkflow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the identifier, text description, and Amazon Resource Name
-- (ARN) for the workflow.
--
-- /See:/ 'newListedWorkflow' smart constructor.
data ListedWorkflow = ListedWorkflow'
  { -- | Specifies the unique Amazon Resource Name (ARN) for the workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the text description for the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the workflow.
    workflowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listedWorkflow_arn' - Specifies the unique Amazon Resource Name (ARN) for the workflow.
--
-- 'description', 'listedWorkflow_description' - Specifies the text description for the workflow.
--
-- 'workflowId', 'listedWorkflow_workflowId' - A unique identifier for the workflow.
newListedWorkflow ::
  ListedWorkflow
newListedWorkflow =
  ListedWorkflow'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      workflowId = Prelude.Nothing
    }

-- | Specifies the unique Amazon Resource Name (ARN) for the workflow.
listedWorkflow_arn :: Lens.Lens' ListedWorkflow (Prelude.Maybe Prelude.Text)
listedWorkflow_arn = Lens.lens (\ListedWorkflow' {arn} -> arn) (\s@ListedWorkflow' {} a -> s {arn = a} :: ListedWorkflow)

-- | Specifies the text description for the workflow.
listedWorkflow_description :: Lens.Lens' ListedWorkflow (Prelude.Maybe Prelude.Text)
listedWorkflow_description = Lens.lens (\ListedWorkflow' {description} -> description) (\s@ListedWorkflow' {} a -> s {description = a} :: ListedWorkflow)

-- | A unique identifier for the workflow.
listedWorkflow_workflowId :: Lens.Lens' ListedWorkflow (Prelude.Maybe Prelude.Text)
listedWorkflow_workflowId = Lens.lens (\ListedWorkflow' {workflowId} -> workflowId) (\s@ListedWorkflow' {} a -> s {workflowId = a} :: ListedWorkflow)

instance Data.FromJSON ListedWorkflow where
  parseJSON =
    Data.withObject
      "ListedWorkflow"
      ( \x ->
          ListedWorkflow'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "WorkflowId")
      )

instance Prelude.Hashable ListedWorkflow where
  hashWithSalt _salt ListedWorkflow' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData ListedWorkflow where
  rnf ListedWorkflow' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workflowId
