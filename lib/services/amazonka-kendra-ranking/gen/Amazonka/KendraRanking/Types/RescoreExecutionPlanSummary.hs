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
-- Module      : Amazonka.KendraRanking.Types.RescoreExecutionPlanSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KendraRanking.Types.RescoreExecutionPlanSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KendraRanking.Types.RescoreExecutionPlanStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information for a rescore execution plan. A rescore execution
-- plan is an Amazon Kendra Intelligent Ranking resource used for
-- provisioning the @Rescore@ API.
--
-- /See:/ 'newRescoreExecutionPlanSummary' smart constructor.
data RescoreExecutionPlanSummary = RescoreExecutionPlanSummary'
  { -- | The Unix timestamp when the rescore execution plan was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the rescore execution plan.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the rescore execution plan.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the rescore execution plan. When the value is
    -- @ACTIVE@, the rescore execution plan is ready for use.
    status :: Prelude.Maybe RescoreExecutionPlanStatus,
    -- | The Unix timestamp when the rescore execution plan was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RescoreExecutionPlanSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'rescoreExecutionPlanSummary_createdAt' - The Unix timestamp when the rescore execution plan was created.
--
-- 'id', 'rescoreExecutionPlanSummary_id' - The identifier of the rescore execution plan.
--
-- 'name', 'rescoreExecutionPlanSummary_name' - The name of the rescore execution plan.
--
-- 'status', 'rescoreExecutionPlanSummary_status' - The current status of the rescore execution plan. When the value is
-- @ACTIVE@, the rescore execution plan is ready for use.
--
-- 'updatedAt', 'rescoreExecutionPlanSummary_updatedAt' - The Unix timestamp when the rescore execution plan was last updated.
newRescoreExecutionPlanSummary ::
  RescoreExecutionPlanSummary
newRescoreExecutionPlanSummary =
  RescoreExecutionPlanSummary'
    { createdAt =
        Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Unix timestamp when the rescore execution plan was created.
rescoreExecutionPlanSummary_createdAt :: Lens.Lens' RescoreExecutionPlanSummary (Prelude.Maybe Prelude.UTCTime)
rescoreExecutionPlanSummary_createdAt = Lens.lens (\RescoreExecutionPlanSummary' {createdAt} -> createdAt) (\s@RescoreExecutionPlanSummary' {} a -> s {createdAt = a} :: RescoreExecutionPlanSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the rescore execution plan.
rescoreExecutionPlanSummary_id :: Lens.Lens' RescoreExecutionPlanSummary (Prelude.Maybe Prelude.Text)
rescoreExecutionPlanSummary_id = Lens.lens (\RescoreExecutionPlanSummary' {id} -> id) (\s@RescoreExecutionPlanSummary' {} a -> s {id = a} :: RescoreExecutionPlanSummary)

-- | The name of the rescore execution plan.
rescoreExecutionPlanSummary_name :: Lens.Lens' RescoreExecutionPlanSummary (Prelude.Maybe Prelude.Text)
rescoreExecutionPlanSummary_name = Lens.lens (\RescoreExecutionPlanSummary' {name} -> name) (\s@RescoreExecutionPlanSummary' {} a -> s {name = a} :: RescoreExecutionPlanSummary)

-- | The current status of the rescore execution plan. When the value is
-- @ACTIVE@, the rescore execution plan is ready for use.
rescoreExecutionPlanSummary_status :: Lens.Lens' RescoreExecutionPlanSummary (Prelude.Maybe RescoreExecutionPlanStatus)
rescoreExecutionPlanSummary_status = Lens.lens (\RescoreExecutionPlanSummary' {status} -> status) (\s@RescoreExecutionPlanSummary' {} a -> s {status = a} :: RescoreExecutionPlanSummary)

-- | The Unix timestamp when the rescore execution plan was last updated.
rescoreExecutionPlanSummary_updatedAt :: Lens.Lens' RescoreExecutionPlanSummary (Prelude.Maybe Prelude.UTCTime)
rescoreExecutionPlanSummary_updatedAt = Lens.lens (\RescoreExecutionPlanSummary' {updatedAt} -> updatedAt) (\s@RescoreExecutionPlanSummary' {} a -> s {updatedAt = a} :: RescoreExecutionPlanSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON RescoreExecutionPlanSummary where
  parseJSON =
    Data.withObject
      "RescoreExecutionPlanSummary"
      ( \x ->
          RescoreExecutionPlanSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable RescoreExecutionPlanSummary where
  hashWithSalt _salt RescoreExecutionPlanSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData RescoreExecutionPlanSummary where
  rnf RescoreExecutionPlanSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
