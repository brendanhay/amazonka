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
-- Module      : Amazonka.SecurityHub.Types.FindingHistoryRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FindingHistoryRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSecurityFindingIdentifier
import Amazonka.SecurityHub.Types.FindingHistoryUpdate
import Amazonka.SecurityHub.Types.FindingHistoryUpdateSource

-- | A list of events that changed the specified finding during the specified
-- time period. Each record represents a single finding change event.
--
-- /See:/ 'newFindingHistoryRecord' smart constructor.
data FindingHistoryRecord = FindingHistoryRecord'
  { -- | Identifies whether the event marks the creation of a new finding. A
    -- value of @True@ means that the finding is newly created. A value of
    -- @False@ means that the finding isn’t newly created.
    findingCreated :: Prelude.Maybe Prelude.Bool,
    findingIdentifier :: Prelude.Maybe AwsSecurityFindingIdentifier,
    -- | A token for pagination purposes. Provide this token in the subsequent
    -- request to
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_GetFindingsHistory.html GetFindingsHistory>
    -- to get up to an additional 100 results of history for the same finding
    -- that you specified in your initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Identifies the source of the event that changed the finding. For
    -- example, an integrated Amazon Web Service or third-party partner
    -- integration may call
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
    -- , or an Security Hub customer may call
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
    -- .
    updateSource :: Prelude.Maybe FindingHistoryUpdateSource,
    -- | An ISO 8601-formatted timestamp that indicates when Security Hub
    -- processed the updated finding record.
    --
    -- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
    -- cannot contain spaces, and date and time should be separated by @T@. For
    -- more information, see
    -- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    updateTime :: Prelude.Maybe Data.ISO8601,
    -- | An array of objects that provides details about the finding change
    -- event, including the Amazon Web Services Security Finding Format (ASFF)
    -- field that changed, the value of the field before the change, and the
    -- value of the field after the change.
    updates :: Prelude.Maybe [FindingHistoryUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingHistoryRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingCreated', 'findingHistoryRecord_findingCreated' - Identifies whether the event marks the creation of a new finding. A
-- value of @True@ means that the finding is newly created. A value of
-- @False@ means that the finding isn’t newly created.
--
-- 'findingIdentifier', 'findingHistoryRecord_findingIdentifier' - Undocumented member.
--
-- 'nextToken', 'findingHistoryRecord_nextToken' - A token for pagination purposes. Provide this token in the subsequent
-- request to
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_GetFindingsHistory.html GetFindingsHistory>
-- to get up to an additional 100 results of history for the same finding
-- that you specified in your initial request.
--
-- 'updateSource', 'findingHistoryRecord_updateSource' - Identifies the source of the event that changed the finding. For
-- example, an integrated Amazon Web Service or third-party partner
-- integration may call
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
-- , or an Security Hub customer may call
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
-- .
--
-- 'updateTime', 'findingHistoryRecord_updateTime' - An ISO 8601-formatted timestamp that indicates when Security Hub
-- processed the updated finding record.
--
-- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
-- cannot contain spaces, and date and time should be separated by @T@. For
-- more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
--
-- 'updates', 'findingHistoryRecord_updates' - An array of objects that provides details about the finding change
-- event, including the Amazon Web Services Security Finding Format (ASFF)
-- field that changed, the value of the field before the change, and the
-- value of the field after the change.
newFindingHistoryRecord ::
  FindingHistoryRecord
newFindingHistoryRecord =
  FindingHistoryRecord'
    { findingCreated =
        Prelude.Nothing,
      findingIdentifier = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      updateSource = Prelude.Nothing,
      updateTime = Prelude.Nothing,
      updates = Prelude.Nothing
    }

-- | Identifies whether the event marks the creation of a new finding. A
-- value of @True@ means that the finding is newly created. A value of
-- @False@ means that the finding isn’t newly created.
findingHistoryRecord_findingCreated :: Lens.Lens' FindingHistoryRecord (Prelude.Maybe Prelude.Bool)
findingHistoryRecord_findingCreated = Lens.lens (\FindingHistoryRecord' {findingCreated} -> findingCreated) (\s@FindingHistoryRecord' {} a -> s {findingCreated = a} :: FindingHistoryRecord)

-- | Undocumented member.
findingHistoryRecord_findingIdentifier :: Lens.Lens' FindingHistoryRecord (Prelude.Maybe AwsSecurityFindingIdentifier)
findingHistoryRecord_findingIdentifier = Lens.lens (\FindingHistoryRecord' {findingIdentifier} -> findingIdentifier) (\s@FindingHistoryRecord' {} a -> s {findingIdentifier = a} :: FindingHistoryRecord)

-- | A token for pagination purposes. Provide this token in the subsequent
-- request to
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_GetFindingsHistory.html GetFindingsHistory>
-- to get up to an additional 100 results of history for the same finding
-- that you specified in your initial request.
findingHistoryRecord_nextToken :: Lens.Lens' FindingHistoryRecord (Prelude.Maybe Prelude.Text)
findingHistoryRecord_nextToken = Lens.lens (\FindingHistoryRecord' {nextToken} -> nextToken) (\s@FindingHistoryRecord' {} a -> s {nextToken = a} :: FindingHistoryRecord)

-- | Identifies the source of the event that changed the finding. For
-- example, an integrated Amazon Web Service or third-party partner
-- integration may call
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
-- , or an Security Hub customer may call
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
-- .
findingHistoryRecord_updateSource :: Lens.Lens' FindingHistoryRecord (Prelude.Maybe FindingHistoryUpdateSource)
findingHistoryRecord_updateSource = Lens.lens (\FindingHistoryRecord' {updateSource} -> updateSource) (\s@FindingHistoryRecord' {} a -> s {updateSource = a} :: FindingHistoryRecord)

-- | An ISO 8601-formatted timestamp that indicates when Security Hub
-- processed the updated finding record.
--
-- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
-- cannot contain spaces, and date and time should be separated by @T@. For
-- more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
findingHistoryRecord_updateTime :: Lens.Lens' FindingHistoryRecord (Prelude.Maybe Prelude.UTCTime)
findingHistoryRecord_updateTime = Lens.lens (\FindingHistoryRecord' {updateTime} -> updateTime) (\s@FindingHistoryRecord' {} a -> s {updateTime = a} :: FindingHistoryRecord) Prelude.. Lens.mapping Data._Time

-- | An array of objects that provides details about the finding change
-- event, including the Amazon Web Services Security Finding Format (ASFF)
-- field that changed, the value of the field before the change, and the
-- value of the field after the change.
findingHistoryRecord_updates :: Lens.Lens' FindingHistoryRecord (Prelude.Maybe [FindingHistoryUpdate])
findingHistoryRecord_updates = Lens.lens (\FindingHistoryRecord' {updates} -> updates) (\s@FindingHistoryRecord' {} a -> s {updates = a} :: FindingHistoryRecord) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FindingHistoryRecord where
  parseJSON =
    Data.withObject
      "FindingHistoryRecord"
      ( \x ->
          FindingHistoryRecord'
            Prelude.<$> (x Data..:? "FindingCreated")
            Prelude.<*> (x Data..:? "FindingIdentifier")
            Prelude.<*> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "UpdateSource")
            Prelude.<*> (x Data..:? "UpdateTime")
            Prelude.<*> (x Data..:? "Updates" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FindingHistoryRecord where
  hashWithSalt _salt FindingHistoryRecord' {..} =
    _salt
      `Prelude.hashWithSalt` findingCreated
      `Prelude.hashWithSalt` findingIdentifier
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` updateSource
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` updates

instance Prelude.NFData FindingHistoryRecord where
  rnf FindingHistoryRecord' {..} =
    Prelude.rnf findingCreated
      `Prelude.seq` Prelude.rnf findingIdentifier
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf updateSource
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf updates
