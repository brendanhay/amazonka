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
-- Module      : Amazonka.OpenSearch.Types.DryRunProgressStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DryRunProgressStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.ValidationFailure
import qualified Amazonka.Prelude as Prelude

-- | Information about the progress of a pre-upgrade dry run analysis.
--
-- /See:/ 'newDryRunProgressStatus' smart constructor.
data DryRunProgressStatus = DryRunProgressStatus'
  { -- | Any validation failures that occurred as a result of the dry run.
    validationFailures :: Prelude.Maybe [ValidationFailure],
    -- | The unique identifier of the dry run.
    dryRunId :: Prelude.Text,
    -- | The current status of the dry run.
    dryRunStatus :: Prelude.Text,
    -- | The timestamp when the dry run was initiated.
    creationDate :: Prelude.Text,
    -- | The timestamp when the dry run was last updated.
    updateDate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DryRunProgressStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationFailures', 'dryRunProgressStatus_validationFailures' - Any validation failures that occurred as a result of the dry run.
--
-- 'dryRunId', 'dryRunProgressStatus_dryRunId' - The unique identifier of the dry run.
--
-- 'dryRunStatus', 'dryRunProgressStatus_dryRunStatus' - The current status of the dry run.
--
-- 'creationDate', 'dryRunProgressStatus_creationDate' - The timestamp when the dry run was initiated.
--
-- 'updateDate', 'dryRunProgressStatus_updateDate' - The timestamp when the dry run was last updated.
newDryRunProgressStatus ::
  -- | 'dryRunId'
  Prelude.Text ->
  -- | 'dryRunStatus'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'updateDate'
  Prelude.Text ->
  DryRunProgressStatus
newDryRunProgressStatus
  pDryRunId_
  pDryRunStatus_
  pCreationDate_
  pUpdateDate_ =
    DryRunProgressStatus'
      { validationFailures =
          Prelude.Nothing,
        dryRunId = pDryRunId_,
        dryRunStatus = pDryRunStatus_,
        creationDate = pCreationDate_,
        updateDate = pUpdateDate_
      }

-- | Any validation failures that occurred as a result of the dry run.
dryRunProgressStatus_validationFailures :: Lens.Lens' DryRunProgressStatus (Prelude.Maybe [ValidationFailure])
dryRunProgressStatus_validationFailures = Lens.lens (\DryRunProgressStatus' {validationFailures} -> validationFailures) (\s@DryRunProgressStatus' {} a -> s {validationFailures = a} :: DryRunProgressStatus) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the dry run.
dryRunProgressStatus_dryRunId :: Lens.Lens' DryRunProgressStatus Prelude.Text
dryRunProgressStatus_dryRunId = Lens.lens (\DryRunProgressStatus' {dryRunId} -> dryRunId) (\s@DryRunProgressStatus' {} a -> s {dryRunId = a} :: DryRunProgressStatus)

-- | The current status of the dry run.
dryRunProgressStatus_dryRunStatus :: Lens.Lens' DryRunProgressStatus Prelude.Text
dryRunProgressStatus_dryRunStatus = Lens.lens (\DryRunProgressStatus' {dryRunStatus} -> dryRunStatus) (\s@DryRunProgressStatus' {} a -> s {dryRunStatus = a} :: DryRunProgressStatus)

-- | The timestamp when the dry run was initiated.
dryRunProgressStatus_creationDate :: Lens.Lens' DryRunProgressStatus Prelude.Text
dryRunProgressStatus_creationDate = Lens.lens (\DryRunProgressStatus' {creationDate} -> creationDate) (\s@DryRunProgressStatus' {} a -> s {creationDate = a} :: DryRunProgressStatus)

-- | The timestamp when the dry run was last updated.
dryRunProgressStatus_updateDate :: Lens.Lens' DryRunProgressStatus Prelude.Text
dryRunProgressStatus_updateDate = Lens.lens (\DryRunProgressStatus' {updateDate} -> updateDate) (\s@DryRunProgressStatus' {} a -> s {updateDate = a} :: DryRunProgressStatus)

instance Data.FromJSON DryRunProgressStatus where
  parseJSON =
    Data.withObject
      "DryRunProgressStatus"
      ( \x ->
          DryRunProgressStatus'
            Prelude.<$> ( x
                            Data..:? "ValidationFailures"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "DryRunId")
            Prelude.<*> (x Data..: "DryRunStatus")
            Prelude.<*> (x Data..: "CreationDate")
            Prelude.<*> (x Data..: "UpdateDate")
      )

instance Prelude.Hashable DryRunProgressStatus where
  hashWithSalt _salt DryRunProgressStatus' {..} =
    _salt
      `Prelude.hashWithSalt` validationFailures
      `Prelude.hashWithSalt` dryRunId
      `Prelude.hashWithSalt` dryRunStatus
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` updateDate

instance Prelude.NFData DryRunProgressStatus where
  rnf DryRunProgressStatus' {..} =
    Prelude.rnf validationFailures
      `Prelude.seq` Prelude.rnf dryRunId
      `Prelude.seq` Prelude.rnf dryRunStatus
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf updateDate
