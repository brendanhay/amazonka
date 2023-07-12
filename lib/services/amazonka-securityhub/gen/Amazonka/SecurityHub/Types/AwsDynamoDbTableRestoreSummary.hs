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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableRestoreSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableRestoreSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the restore for the table.
--
-- /See:/ 'newAwsDynamoDbTableRestoreSummary' smart constructor.
data AwsDynamoDbTableRestoreSummary = AwsDynamoDbTableRestoreSummary'
  { -- | Indicates the point in time that the table was restored to.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    restoreDateTime :: Prelude.Maybe Prelude.Text,
    -- | Whether a restore is currently in progress.
    restoreInProgress :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the source backup from which the table was restored.
    sourceBackupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source table for the backup.
    sourceTableArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableRestoreSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restoreDateTime', 'awsDynamoDbTableRestoreSummary_restoreDateTime' - Indicates the point in time that the table was restored to.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'restoreInProgress', 'awsDynamoDbTableRestoreSummary_restoreInProgress' - Whether a restore is currently in progress.
--
-- 'sourceBackupArn', 'awsDynamoDbTableRestoreSummary_sourceBackupArn' - The ARN of the source backup from which the table was restored.
--
-- 'sourceTableArn', 'awsDynamoDbTableRestoreSummary_sourceTableArn' - The ARN of the source table for the backup.
newAwsDynamoDbTableRestoreSummary ::
  AwsDynamoDbTableRestoreSummary
newAwsDynamoDbTableRestoreSummary =
  AwsDynamoDbTableRestoreSummary'
    { restoreDateTime =
        Prelude.Nothing,
      restoreInProgress = Prelude.Nothing,
      sourceBackupArn = Prelude.Nothing,
      sourceTableArn = Prelude.Nothing
    }

-- | Indicates the point in time that the table was restored to.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsDynamoDbTableRestoreSummary_restoreDateTime :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableRestoreSummary_restoreDateTime = Lens.lens (\AwsDynamoDbTableRestoreSummary' {restoreDateTime} -> restoreDateTime) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {restoreDateTime = a} :: AwsDynamoDbTableRestoreSummary)

-- | Whether a restore is currently in progress.
awsDynamoDbTableRestoreSummary_restoreInProgress :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Bool)
awsDynamoDbTableRestoreSummary_restoreInProgress = Lens.lens (\AwsDynamoDbTableRestoreSummary' {restoreInProgress} -> restoreInProgress) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {restoreInProgress = a} :: AwsDynamoDbTableRestoreSummary)

-- | The ARN of the source backup from which the table was restored.
awsDynamoDbTableRestoreSummary_sourceBackupArn :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableRestoreSummary_sourceBackupArn = Lens.lens (\AwsDynamoDbTableRestoreSummary' {sourceBackupArn} -> sourceBackupArn) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {sourceBackupArn = a} :: AwsDynamoDbTableRestoreSummary)

-- | The ARN of the source table for the backup.
awsDynamoDbTableRestoreSummary_sourceTableArn :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableRestoreSummary_sourceTableArn = Lens.lens (\AwsDynamoDbTableRestoreSummary' {sourceTableArn} -> sourceTableArn) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {sourceTableArn = a} :: AwsDynamoDbTableRestoreSummary)

instance Data.FromJSON AwsDynamoDbTableRestoreSummary where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableRestoreSummary"
      ( \x ->
          AwsDynamoDbTableRestoreSummary'
            Prelude.<$> (x Data..:? "RestoreDateTime")
            Prelude.<*> (x Data..:? "RestoreInProgress")
            Prelude.<*> (x Data..:? "SourceBackupArn")
            Prelude.<*> (x Data..:? "SourceTableArn")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableRestoreSummary
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableRestoreSummary' {..} =
      _salt
        `Prelude.hashWithSalt` restoreDateTime
        `Prelude.hashWithSalt` restoreInProgress
        `Prelude.hashWithSalt` sourceBackupArn
        `Prelude.hashWithSalt` sourceTableArn

instance
  Prelude.NFData
    AwsDynamoDbTableRestoreSummary
  where
  rnf AwsDynamoDbTableRestoreSummary' {..} =
    Prelude.rnf restoreDateTime
      `Prelude.seq` Prelude.rnf restoreInProgress
      `Prelude.seq` Prelude.rnf sourceBackupArn
      `Prelude.seq` Prelude.rnf sourceTableArn

instance Data.ToJSON AwsDynamoDbTableRestoreSummary where
  toJSON AwsDynamoDbTableRestoreSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RestoreDateTime" Data..=)
              Prelude.<$> restoreDateTime,
            ("RestoreInProgress" Data..=)
              Prelude.<$> restoreInProgress,
            ("SourceBackupArn" Data..=)
              Prelude.<$> sourceBackupArn,
            ("SourceTableArn" Data..=)
              Prelude.<$> sourceTableArn
          ]
      )
