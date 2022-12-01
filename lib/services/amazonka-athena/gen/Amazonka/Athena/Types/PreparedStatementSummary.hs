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
-- Module      : Amazonka.Athena.Types.PreparedStatementSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.PreparedStatementSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The name and last modified time of the prepared statement.
--
-- /See:/ 'newPreparedStatementSummary' smart constructor.
data PreparedStatementSummary = PreparedStatementSummary'
  { -- | The last modified time of the prepared statement.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the prepared statement.
    statementName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PreparedStatementSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'preparedStatementSummary_lastModifiedTime' - The last modified time of the prepared statement.
--
-- 'statementName', 'preparedStatementSummary_statementName' - The name of the prepared statement.
newPreparedStatementSummary ::
  PreparedStatementSummary
newPreparedStatementSummary =
  PreparedStatementSummary'
    { lastModifiedTime =
        Prelude.Nothing,
      statementName = Prelude.Nothing
    }

-- | The last modified time of the prepared statement.
preparedStatementSummary_lastModifiedTime :: Lens.Lens' PreparedStatementSummary (Prelude.Maybe Prelude.UTCTime)
preparedStatementSummary_lastModifiedTime = Lens.lens (\PreparedStatementSummary' {lastModifiedTime} -> lastModifiedTime) (\s@PreparedStatementSummary' {} a -> s {lastModifiedTime = a} :: PreparedStatementSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the prepared statement.
preparedStatementSummary_statementName :: Lens.Lens' PreparedStatementSummary (Prelude.Maybe Prelude.Text)
preparedStatementSummary_statementName = Lens.lens (\PreparedStatementSummary' {statementName} -> statementName) (\s@PreparedStatementSummary' {} a -> s {statementName = a} :: PreparedStatementSummary)

instance Core.FromJSON PreparedStatementSummary where
  parseJSON =
    Core.withObject
      "PreparedStatementSummary"
      ( \x ->
          PreparedStatementSummary'
            Prelude.<$> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "StatementName")
      )

instance Prelude.Hashable PreparedStatementSummary where
  hashWithSalt _salt PreparedStatementSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` statementName

instance Prelude.NFData PreparedStatementSummary where
  rnf PreparedStatementSummary' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf statementName
