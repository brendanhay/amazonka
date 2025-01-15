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
-- Module      : Amazonka.Omics.Types.WorkflowListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.WorkflowListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.WorkflowStatus
import Amazonka.Omics.Types.WorkflowType
import qualified Amazonka.Prelude as Prelude

-- | A workflow.
--
-- /See:/ 'newWorkflowListItem' smart constructor.
data WorkflowListItem = WorkflowListItem'
  { -- | The workflow\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the workflow was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The workflow\'s digest.
    digest :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s status.
    status :: Prelude.Maybe WorkflowStatus,
    -- | The workflow\'s type.
    type' :: Prelude.Maybe WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'workflowListItem_arn' - The workflow\'s ARN.
--
-- 'creationTime', 'workflowListItem_creationTime' - When the workflow was created.
--
-- 'digest', 'workflowListItem_digest' - The workflow\'s digest.
--
-- 'id', 'workflowListItem_id' - The workflow\'s ID.
--
-- 'name', 'workflowListItem_name' - The workflow\'s name.
--
-- 'status', 'workflowListItem_status' - The workflow\'s status.
--
-- 'type'', 'workflowListItem_type' - The workflow\'s type.
newWorkflowListItem ::
  WorkflowListItem
newWorkflowListItem =
  WorkflowListItem'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      digest = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The workflow\'s ARN.
workflowListItem_arn :: Lens.Lens' WorkflowListItem (Prelude.Maybe Prelude.Text)
workflowListItem_arn = Lens.lens (\WorkflowListItem' {arn} -> arn) (\s@WorkflowListItem' {} a -> s {arn = a} :: WorkflowListItem)

-- | When the workflow was created.
workflowListItem_creationTime :: Lens.Lens' WorkflowListItem (Prelude.Maybe Prelude.UTCTime)
workflowListItem_creationTime = Lens.lens (\WorkflowListItem' {creationTime} -> creationTime) (\s@WorkflowListItem' {} a -> s {creationTime = a} :: WorkflowListItem) Prelude.. Lens.mapping Data._Time

-- | The workflow\'s digest.
workflowListItem_digest :: Lens.Lens' WorkflowListItem (Prelude.Maybe Prelude.Text)
workflowListItem_digest = Lens.lens (\WorkflowListItem' {digest} -> digest) (\s@WorkflowListItem' {} a -> s {digest = a} :: WorkflowListItem)

-- | The workflow\'s ID.
workflowListItem_id :: Lens.Lens' WorkflowListItem (Prelude.Maybe Prelude.Text)
workflowListItem_id = Lens.lens (\WorkflowListItem' {id} -> id) (\s@WorkflowListItem' {} a -> s {id = a} :: WorkflowListItem)

-- | The workflow\'s name.
workflowListItem_name :: Lens.Lens' WorkflowListItem (Prelude.Maybe Prelude.Text)
workflowListItem_name = Lens.lens (\WorkflowListItem' {name} -> name) (\s@WorkflowListItem' {} a -> s {name = a} :: WorkflowListItem)

-- | The workflow\'s status.
workflowListItem_status :: Lens.Lens' WorkflowListItem (Prelude.Maybe WorkflowStatus)
workflowListItem_status = Lens.lens (\WorkflowListItem' {status} -> status) (\s@WorkflowListItem' {} a -> s {status = a} :: WorkflowListItem)

-- | The workflow\'s type.
workflowListItem_type :: Lens.Lens' WorkflowListItem (Prelude.Maybe WorkflowType)
workflowListItem_type = Lens.lens (\WorkflowListItem' {type'} -> type') (\s@WorkflowListItem' {} a -> s {type' = a} :: WorkflowListItem)

instance Data.FromJSON WorkflowListItem where
  parseJSON =
    Data.withObject
      "WorkflowListItem"
      ( \x ->
          WorkflowListItem'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "digest")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable WorkflowListItem where
  hashWithSalt _salt WorkflowListItem' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` digest
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData WorkflowListItem where
  rnf WorkflowListItem' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf digest `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf type'
