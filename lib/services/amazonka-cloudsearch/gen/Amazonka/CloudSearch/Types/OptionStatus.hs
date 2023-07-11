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
-- Module      : Amazonka.CloudSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.OptionStatus where

import Amazonka.CloudSearch.Types.OptionState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of domain configuration option.
--
-- /See:/ 'newOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { -- | Indicates that the option will be deleted once processing is complete.
    pendingDeletion :: Prelude.Maybe Prelude.Bool,
    -- | A unique integer that indicates when this option was last updated.
    updateVersion :: Prelude.Maybe Prelude.Natural,
    -- | A timestamp for when this option was created.
    creationDate :: Data.ISO8601,
    -- | A timestamp for when this option was last updated.
    updateDate :: Data.ISO8601,
    -- | The state of processing a change to an option. Possible values:
    --
    -- -   @RequiresIndexDocuments@: the option\'s latest value will not be
    --     deployed until IndexDocuments has been called and indexing is
    --     complete.
    -- -   @Processing@: the option\'s latest value is in the process of being
    --     activated.
    -- -   @Active@: the option\'s latest value is completely deployed.
    -- -   @FailedToValidate@: the option value is not compatible with the
    --     domain\'s data and cannot be used to index the data. You must either
    --     modify the option value or update or remove the incompatible
    --     documents.
    state :: OptionState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingDeletion', 'optionStatus_pendingDeletion' - Indicates that the option will be deleted once processing is complete.
--
-- 'updateVersion', 'optionStatus_updateVersion' - A unique integer that indicates when this option was last updated.
--
-- 'creationDate', 'optionStatus_creationDate' - A timestamp for when this option was created.
--
-- 'updateDate', 'optionStatus_updateDate' - A timestamp for when this option was last updated.
--
-- 'state', 'optionStatus_state' - The state of processing a change to an option. Possible values:
--
-- -   @RequiresIndexDocuments@: the option\'s latest value will not be
--     deployed until IndexDocuments has been called and indexing is
--     complete.
-- -   @Processing@: the option\'s latest value is in the process of being
--     activated.
-- -   @Active@: the option\'s latest value is completely deployed.
-- -   @FailedToValidate@: the option value is not compatible with the
--     domain\'s data and cannot be used to index the data. You must either
--     modify the option value or update or remove the incompatible
--     documents.
newOptionStatus ::
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'updateDate'
  Prelude.UTCTime ->
  -- | 'state'
  OptionState ->
  OptionStatus
newOptionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { pendingDeletion = Prelude.Nothing,
      updateVersion = Prelude.Nothing,
      creationDate = Data._Time Lens.# pCreationDate_,
      updateDate = Data._Time Lens.# pUpdateDate_,
      state = pState_
    }

-- | Indicates that the option will be deleted once processing is complete.
optionStatus_pendingDeletion :: Lens.Lens' OptionStatus (Prelude.Maybe Prelude.Bool)
optionStatus_pendingDeletion = Lens.lens (\OptionStatus' {pendingDeletion} -> pendingDeletion) (\s@OptionStatus' {} a -> s {pendingDeletion = a} :: OptionStatus)

-- | A unique integer that indicates when this option was last updated.
optionStatus_updateVersion :: Lens.Lens' OptionStatus (Prelude.Maybe Prelude.Natural)
optionStatus_updateVersion = Lens.lens (\OptionStatus' {updateVersion} -> updateVersion) (\s@OptionStatus' {} a -> s {updateVersion = a} :: OptionStatus)

-- | A timestamp for when this option was created.
optionStatus_creationDate :: Lens.Lens' OptionStatus Prelude.UTCTime
optionStatus_creationDate = Lens.lens (\OptionStatus' {creationDate} -> creationDate) (\s@OptionStatus' {} a -> s {creationDate = a} :: OptionStatus) Prelude.. Data._Time

-- | A timestamp for when this option was last updated.
optionStatus_updateDate :: Lens.Lens' OptionStatus Prelude.UTCTime
optionStatus_updateDate = Lens.lens (\OptionStatus' {updateDate} -> updateDate) (\s@OptionStatus' {} a -> s {updateDate = a} :: OptionStatus) Prelude.. Data._Time

-- | The state of processing a change to an option. Possible values:
--
-- -   @RequiresIndexDocuments@: the option\'s latest value will not be
--     deployed until IndexDocuments has been called and indexing is
--     complete.
-- -   @Processing@: the option\'s latest value is in the process of being
--     activated.
-- -   @Active@: the option\'s latest value is completely deployed.
-- -   @FailedToValidate@: the option value is not compatible with the
--     domain\'s data and cannot be used to index the data. You must either
--     modify the option value or update or remove the incompatible
--     documents.
optionStatus_state :: Lens.Lens' OptionStatus OptionState
optionStatus_state = Lens.lens (\OptionStatus' {state} -> state) (\s@OptionStatus' {} a -> s {state = a} :: OptionStatus)

instance Data.FromXML OptionStatus where
  parseXML x =
    OptionStatus'
      Prelude.<$> (x Data..@? "PendingDeletion")
      Prelude.<*> (x Data..@? "UpdateVersion")
      Prelude.<*> (x Data..@ "CreationDate")
      Prelude.<*> (x Data..@ "UpdateDate")
      Prelude.<*> (x Data..@ "State")

instance Prelude.Hashable OptionStatus where
  hashWithSalt _salt OptionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` pendingDeletion
      `Prelude.hashWithSalt` updateVersion
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` updateDate
      `Prelude.hashWithSalt` state

instance Prelude.NFData OptionStatus where
  rnf OptionStatus' {..} =
    Prelude.rnf pendingDeletion
      `Prelude.seq` Prelude.rnf updateVersion
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf updateDate
      `Prelude.seq` Prelude.rnf state
