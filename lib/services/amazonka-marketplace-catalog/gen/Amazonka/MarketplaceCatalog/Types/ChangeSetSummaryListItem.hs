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
-- Module      : Amazonka.MarketplaceCatalog.Types.ChangeSetSummaryListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types.ChangeSetSummaryListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types.ChangeStatus
import Amazonka.MarketplaceCatalog.Types.FailureCode
import qualified Amazonka.Prelude as Prelude

-- | A summary of a change set returned in a list of change sets when the
-- @ListChangeSets@ action is called.
--
-- /See:/ 'newChangeSetSummaryListItem' smart constructor.
data ChangeSetSummaryListItem = ChangeSetSummaryListItem'
  { -- | The ARN associated with the unique identifier for the change set
    -- referenced in this request.
    changeSetArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a change set.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The non-unique name for the change set.
    changeSetName :: Prelude.Maybe Prelude.Text,
    -- | The time, in ISO 8601 format (2018-02-27T13:45:22Z), when the change set
    -- was finished.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | This object is a list of entity IDs (string) that are a part of a change
    -- set. The entity ID list is a maximum of 20 entities. It must contain at
    -- least one entity.
    entityIdList :: Prelude.Maybe [Prelude.Text],
    -- | Returned if the change set is in @FAILED@ status. Can be either
    -- @CLIENT_ERROR@, which means that there are issues with the request (see
    -- the @ErrorDetailList@ of @DescribeChangeSet@), or @SERVER_FAULT@, which
    -- means that there is a problem in the system, and you should retry your
    -- request.
    failureCode :: Prelude.Maybe FailureCode,
    -- | The time, in ISO 8601 format (2018-02-27T13:45:22Z), when the change set
    -- was started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The current status of the change set.
    status :: Prelude.Maybe ChangeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeSetSummaryListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeSetArn', 'changeSetSummaryListItem_changeSetArn' - The ARN associated with the unique identifier for the change set
-- referenced in this request.
--
-- 'changeSetId', 'changeSetSummaryListItem_changeSetId' - The unique identifier for a change set.
--
-- 'changeSetName', 'changeSetSummaryListItem_changeSetName' - The non-unique name for the change set.
--
-- 'endTime', 'changeSetSummaryListItem_endTime' - The time, in ISO 8601 format (2018-02-27T13:45:22Z), when the change set
-- was finished.
--
-- 'entityIdList', 'changeSetSummaryListItem_entityIdList' - This object is a list of entity IDs (string) that are a part of a change
-- set. The entity ID list is a maximum of 20 entities. It must contain at
-- least one entity.
--
-- 'failureCode', 'changeSetSummaryListItem_failureCode' - Returned if the change set is in @FAILED@ status. Can be either
-- @CLIENT_ERROR@, which means that there are issues with the request (see
-- the @ErrorDetailList@ of @DescribeChangeSet@), or @SERVER_FAULT@, which
-- means that there is a problem in the system, and you should retry your
-- request.
--
-- 'startTime', 'changeSetSummaryListItem_startTime' - The time, in ISO 8601 format (2018-02-27T13:45:22Z), when the change set
-- was started.
--
-- 'status', 'changeSetSummaryListItem_status' - The current status of the change set.
newChangeSetSummaryListItem ::
  ChangeSetSummaryListItem
newChangeSetSummaryListItem =
  ChangeSetSummaryListItem'
    { changeSetArn =
        Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      changeSetName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      entityIdList = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ARN associated with the unique identifier for the change set
-- referenced in this request.
changeSetSummaryListItem_changeSetArn :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe Prelude.Text)
changeSetSummaryListItem_changeSetArn = Lens.lens (\ChangeSetSummaryListItem' {changeSetArn} -> changeSetArn) (\s@ChangeSetSummaryListItem' {} a -> s {changeSetArn = a} :: ChangeSetSummaryListItem)

-- | The unique identifier for a change set.
changeSetSummaryListItem_changeSetId :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe Prelude.Text)
changeSetSummaryListItem_changeSetId = Lens.lens (\ChangeSetSummaryListItem' {changeSetId} -> changeSetId) (\s@ChangeSetSummaryListItem' {} a -> s {changeSetId = a} :: ChangeSetSummaryListItem)

-- | The non-unique name for the change set.
changeSetSummaryListItem_changeSetName :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe Prelude.Text)
changeSetSummaryListItem_changeSetName = Lens.lens (\ChangeSetSummaryListItem' {changeSetName} -> changeSetName) (\s@ChangeSetSummaryListItem' {} a -> s {changeSetName = a} :: ChangeSetSummaryListItem)

-- | The time, in ISO 8601 format (2018-02-27T13:45:22Z), when the change set
-- was finished.
changeSetSummaryListItem_endTime :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe Prelude.Text)
changeSetSummaryListItem_endTime = Lens.lens (\ChangeSetSummaryListItem' {endTime} -> endTime) (\s@ChangeSetSummaryListItem' {} a -> s {endTime = a} :: ChangeSetSummaryListItem)

-- | This object is a list of entity IDs (string) that are a part of a change
-- set. The entity ID list is a maximum of 20 entities. It must contain at
-- least one entity.
changeSetSummaryListItem_entityIdList :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe [Prelude.Text])
changeSetSummaryListItem_entityIdList = Lens.lens (\ChangeSetSummaryListItem' {entityIdList} -> entityIdList) (\s@ChangeSetSummaryListItem' {} a -> s {entityIdList = a} :: ChangeSetSummaryListItem) Prelude.. Lens.mapping Lens.coerced

-- | Returned if the change set is in @FAILED@ status. Can be either
-- @CLIENT_ERROR@, which means that there are issues with the request (see
-- the @ErrorDetailList@ of @DescribeChangeSet@), or @SERVER_FAULT@, which
-- means that there is a problem in the system, and you should retry your
-- request.
changeSetSummaryListItem_failureCode :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe FailureCode)
changeSetSummaryListItem_failureCode = Lens.lens (\ChangeSetSummaryListItem' {failureCode} -> failureCode) (\s@ChangeSetSummaryListItem' {} a -> s {failureCode = a} :: ChangeSetSummaryListItem)

-- | The time, in ISO 8601 format (2018-02-27T13:45:22Z), when the change set
-- was started.
changeSetSummaryListItem_startTime :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe Prelude.Text)
changeSetSummaryListItem_startTime = Lens.lens (\ChangeSetSummaryListItem' {startTime} -> startTime) (\s@ChangeSetSummaryListItem' {} a -> s {startTime = a} :: ChangeSetSummaryListItem)

-- | The current status of the change set.
changeSetSummaryListItem_status :: Lens.Lens' ChangeSetSummaryListItem (Prelude.Maybe ChangeStatus)
changeSetSummaryListItem_status = Lens.lens (\ChangeSetSummaryListItem' {status} -> status) (\s@ChangeSetSummaryListItem' {} a -> s {status = a} :: ChangeSetSummaryListItem)

instance Data.FromJSON ChangeSetSummaryListItem where
  parseJSON =
    Data.withObject
      "ChangeSetSummaryListItem"
      ( \x ->
          ChangeSetSummaryListItem'
            Prelude.<$> (x Data..:? "ChangeSetArn")
            Prelude.<*> (x Data..:? "ChangeSetId")
            Prelude.<*> (x Data..:? "ChangeSetName")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "EntityIdList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FailureCode")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ChangeSetSummaryListItem where
  hashWithSalt _salt ChangeSetSummaryListItem' {..} =
    _salt
      `Prelude.hashWithSalt` changeSetArn
      `Prelude.hashWithSalt` changeSetId
      `Prelude.hashWithSalt` changeSetName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` entityIdList
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData ChangeSetSummaryListItem where
  rnf ChangeSetSummaryListItem' {..} =
    Prelude.rnf changeSetArn
      `Prelude.seq` Prelude.rnf changeSetId
      `Prelude.seq` Prelude.rnf changeSetName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf entityIdList
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
