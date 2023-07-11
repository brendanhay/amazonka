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
-- Module      : Amazonka.FinSpaceData.Types.ChangesetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ChangesetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ChangeType
import Amazonka.FinSpaceData.Types.ChangesetErrorInfo
import Amazonka.FinSpaceData.Types.IngestionStatus
import qualified Amazonka.Prelude as Prelude

-- | A Changeset is unit of data in a Dataset.
--
-- /See:/ 'newChangesetSummary' smart constructor.
data ChangesetSummary = ChangesetSummary'
  { -- | Beginning time from which the Changeset is active. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    activeFromTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | Time until which the Changeset is active. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    activeUntilTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | Type that indicates how a Changeset is applied to a Dataset.
    --
    -- -   @REPLACE@ – Changeset is considered as a replacement to all prior
    --     loaded Changesets.
    --
    -- -   @APPEND@ – Changeset is considered as an addition to the end of all
    --     prior loaded Changesets.
    --
    -- -   @MODIFY@ – Changeset is considered as a replacement to a specific
    --     prior ingested Changeset.
    changeType :: Prelude.Maybe ChangeType,
    -- | The ARN identifier of the Changeset.
    changesetArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a Changeset.
    changesetId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the Changeset was created in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier for the FinSpace Dataset in which the Changeset is
    -- created.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The structure with error messages.
    errorInfo :: Prelude.Maybe ChangesetErrorInfo,
    -- | Options that define the structure of the source file(s).
    formatParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Options that define the location of the data being ingested.
    sourceParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Status of the Changeset ingestion.
    --
    -- -   @PENDING@ – Changeset is pending creation.
    --
    -- -   @FAILED@ – Changeset creation has failed.
    --
    -- -   @SUCCESS@ – Changeset creation has succeeded.
    --
    -- -   @RUNNING@ – Changeset creation is running.
    --
    -- -   @STOP_REQUESTED@ – User requested Changeset creation to stop.
    status :: Prelude.Maybe IngestionStatus,
    -- | The unique identifier of the updated Changeset.
    updatedByChangesetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Changeset that is updated.
    updatesChangesetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangesetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeFromTimestamp', 'changesetSummary_activeFromTimestamp' - Beginning time from which the Changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'activeUntilTimestamp', 'changesetSummary_activeUntilTimestamp' - Time until which the Changeset is active. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'changeType', 'changesetSummary_changeType' - Type that indicates how a Changeset is applied to a Dataset.
--
-- -   @REPLACE@ – Changeset is considered as a replacement to all prior
--     loaded Changesets.
--
-- -   @APPEND@ – Changeset is considered as an addition to the end of all
--     prior loaded Changesets.
--
-- -   @MODIFY@ – Changeset is considered as a replacement to a specific
--     prior ingested Changeset.
--
-- 'changesetArn', 'changesetSummary_changesetArn' - The ARN identifier of the Changeset.
--
-- 'changesetId', 'changesetSummary_changesetId' - The unique identifier for a Changeset.
--
-- 'createTime', 'changesetSummary_createTime' - The timestamp at which the Changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'datasetId', 'changesetSummary_datasetId' - The unique identifier for the FinSpace Dataset in which the Changeset is
-- created.
--
-- 'errorInfo', 'changesetSummary_errorInfo' - The structure with error messages.
--
-- 'formatParams', 'changesetSummary_formatParams' - Options that define the structure of the source file(s).
--
-- 'sourceParams', 'changesetSummary_sourceParams' - Options that define the location of the data being ingested.
--
-- 'status', 'changesetSummary_status' - Status of the Changeset ingestion.
--
-- -   @PENDING@ – Changeset is pending creation.
--
-- -   @FAILED@ – Changeset creation has failed.
--
-- -   @SUCCESS@ – Changeset creation has succeeded.
--
-- -   @RUNNING@ – Changeset creation is running.
--
-- -   @STOP_REQUESTED@ – User requested Changeset creation to stop.
--
-- 'updatedByChangesetId', 'changesetSummary_updatedByChangesetId' - The unique identifier of the updated Changeset.
--
-- 'updatesChangesetId', 'changesetSummary_updatesChangesetId' - The unique identifier of the Changeset that is updated.
newChangesetSummary ::
  ChangesetSummary
newChangesetSummary =
  ChangesetSummary'
    { activeFromTimestamp =
        Prelude.Nothing,
      activeUntilTimestamp = Prelude.Nothing,
      changeType = Prelude.Nothing,
      changesetArn = Prelude.Nothing,
      changesetId = Prelude.Nothing,
      createTime = Prelude.Nothing,
      datasetId = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      formatParams = Prelude.Nothing,
      sourceParams = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedByChangesetId = Prelude.Nothing,
      updatesChangesetId = Prelude.Nothing
    }

-- | Beginning time from which the Changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
changesetSummary_activeFromTimestamp :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Integer)
changesetSummary_activeFromTimestamp = Lens.lens (\ChangesetSummary' {activeFromTimestamp} -> activeFromTimestamp) (\s@ChangesetSummary' {} a -> s {activeFromTimestamp = a} :: ChangesetSummary)

-- | Time until which the Changeset is active. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
changesetSummary_activeUntilTimestamp :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Integer)
changesetSummary_activeUntilTimestamp = Lens.lens (\ChangesetSummary' {activeUntilTimestamp} -> activeUntilTimestamp) (\s@ChangesetSummary' {} a -> s {activeUntilTimestamp = a} :: ChangesetSummary)

-- | Type that indicates how a Changeset is applied to a Dataset.
--
-- -   @REPLACE@ – Changeset is considered as a replacement to all prior
--     loaded Changesets.
--
-- -   @APPEND@ – Changeset is considered as an addition to the end of all
--     prior loaded Changesets.
--
-- -   @MODIFY@ – Changeset is considered as a replacement to a specific
--     prior ingested Changeset.
changesetSummary_changeType :: Lens.Lens' ChangesetSummary (Prelude.Maybe ChangeType)
changesetSummary_changeType = Lens.lens (\ChangesetSummary' {changeType} -> changeType) (\s@ChangesetSummary' {} a -> s {changeType = a} :: ChangesetSummary)

-- | The ARN identifier of the Changeset.
changesetSummary_changesetArn :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Text)
changesetSummary_changesetArn = Lens.lens (\ChangesetSummary' {changesetArn} -> changesetArn) (\s@ChangesetSummary' {} a -> s {changesetArn = a} :: ChangesetSummary)

-- | The unique identifier for a Changeset.
changesetSummary_changesetId :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Text)
changesetSummary_changesetId = Lens.lens (\ChangesetSummary' {changesetId} -> changesetId) (\s@ChangesetSummary' {} a -> s {changesetId = a} :: ChangesetSummary)

-- | The timestamp at which the Changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
changesetSummary_createTime :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Integer)
changesetSummary_createTime = Lens.lens (\ChangesetSummary' {createTime} -> createTime) (\s@ChangesetSummary' {} a -> s {createTime = a} :: ChangesetSummary)

-- | The unique identifier for the FinSpace Dataset in which the Changeset is
-- created.
changesetSummary_datasetId :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Text)
changesetSummary_datasetId = Lens.lens (\ChangesetSummary' {datasetId} -> datasetId) (\s@ChangesetSummary' {} a -> s {datasetId = a} :: ChangesetSummary)

-- | The structure with error messages.
changesetSummary_errorInfo :: Lens.Lens' ChangesetSummary (Prelude.Maybe ChangesetErrorInfo)
changesetSummary_errorInfo = Lens.lens (\ChangesetSummary' {errorInfo} -> errorInfo) (\s@ChangesetSummary' {} a -> s {errorInfo = a} :: ChangesetSummary)

-- | Options that define the structure of the source file(s).
changesetSummary_formatParams :: Lens.Lens' ChangesetSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetSummary_formatParams = Lens.lens (\ChangesetSummary' {formatParams} -> formatParams) (\s@ChangesetSummary' {} a -> s {formatParams = a} :: ChangesetSummary) Prelude.. Lens.mapping Lens.coerced

-- | Options that define the location of the data being ingested.
changesetSummary_sourceParams :: Lens.Lens' ChangesetSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetSummary_sourceParams = Lens.lens (\ChangesetSummary' {sourceParams} -> sourceParams) (\s@ChangesetSummary' {} a -> s {sourceParams = a} :: ChangesetSummary) Prelude.. Lens.mapping Lens.coerced

-- | Status of the Changeset ingestion.
--
-- -   @PENDING@ – Changeset is pending creation.
--
-- -   @FAILED@ – Changeset creation has failed.
--
-- -   @SUCCESS@ – Changeset creation has succeeded.
--
-- -   @RUNNING@ – Changeset creation is running.
--
-- -   @STOP_REQUESTED@ – User requested Changeset creation to stop.
changesetSummary_status :: Lens.Lens' ChangesetSummary (Prelude.Maybe IngestionStatus)
changesetSummary_status = Lens.lens (\ChangesetSummary' {status} -> status) (\s@ChangesetSummary' {} a -> s {status = a} :: ChangesetSummary)

-- | The unique identifier of the updated Changeset.
changesetSummary_updatedByChangesetId :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Text)
changesetSummary_updatedByChangesetId = Lens.lens (\ChangesetSummary' {updatedByChangesetId} -> updatedByChangesetId) (\s@ChangesetSummary' {} a -> s {updatedByChangesetId = a} :: ChangesetSummary)

-- | The unique identifier of the Changeset that is updated.
changesetSummary_updatesChangesetId :: Lens.Lens' ChangesetSummary (Prelude.Maybe Prelude.Text)
changesetSummary_updatesChangesetId = Lens.lens (\ChangesetSummary' {updatesChangesetId} -> updatesChangesetId) (\s@ChangesetSummary' {} a -> s {updatesChangesetId = a} :: ChangesetSummary)

instance Data.FromJSON ChangesetSummary where
  parseJSON =
    Data.withObject
      "ChangesetSummary"
      ( \x ->
          ChangesetSummary'
            Prelude.<$> (x Data..:? "activeFromTimestamp")
            Prelude.<*> (x Data..:? "activeUntilTimestamp")
            Prelude.<*> (x Data..:? "changeType")
            Prelude.<*> (x Data..:? "changesetArn")
            Prelude.<*> (x Data..:? "changesetId")
            Prelude.<*> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "datasetId")
            Prelude.<*> (x Data..:? "errorInfo")
            Prelude.<*> (x Data..:? "formatParams" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "sourceParams" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updatedByChangesetId")
            Prelude.<*> (x Data..:? "updatesChangesetId")
      )

instance Prelude.Hashable ChangesetSummary where
  hashWithSalt _salt ChangesetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` activeFromTimestamp
      `Prelude.hashWithSalt` activeUntilTimestamp
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` changesetArn
      `Prelude.hashWithSalt` changesetId
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` formatParams
      `Prelude.hashWithSalt` sourceParams
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedByChangesetId
      `Prelude.hashWithSalt` updatesChangesetId

instance Prelude.NFData ChangesetSummary where
  rnf ChangesetSummary' {..} =
    Prelude.rnf activeFromTimestamp
      `Prelude.seq` Prelude.rnf activeUntilTimestamp
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf changesetArn
      `Prelude.seq` Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf formatParams
      `Prelude.seq` Prelude.rnf sourceParams
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedByChangesetId
      `Prelude.seq` Prelude.rnf updatesChangesetId
