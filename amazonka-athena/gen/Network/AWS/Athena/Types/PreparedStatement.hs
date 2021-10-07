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
-- Module      : Network.AWS.Athena.Types.PreparedStatement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.PreparedStatement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A prepared SQL statement for use with Athena.
--
-- /See:/ 'newPreparedStatement' smart constructor.
data PreparedStatement = PreparedStatement'
  { -- | The name of the workgroup to which the prepared statement belongs.
    workGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the prepared statement.
    statementName :: Prelude.Maybe Prelude.Text,
    -- | The query string for the prepared statement.
    queryStatement :: Prelude.Maybe Prelude.Text,
    -- | The last modified time of the prepared statement.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the prepared statement.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PreparedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workGroupName', 'preparedStatement_workGroupName' - The name of the workgroup to which the prepared statement belongs.
--
-- 'statementName', 'preparedStatement_statementName' - The name of the prepared statement.
--
-- 'queryStatement', 'preparedStatement_queryStatement' - The query string for the prepared statement.
--
-- 'lastModifiedTime', 'preparedStatement_lastModifiedTime' - The last modified time of the prepared statement.
--
-- 'description', 'preparedStatement_description' - The description of the prepared statement.
newPreparedStatement ::
  PreparedStatement
newPreparedStatement =
  PreparedStatement'
    { workGroupName = Prelude.Nothing,
      statementName = Prelude.Nothing,
      queryStatement = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The name of the workgroup to which the prepared statement belongs.
preparedStatement_workGroupName :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_workGroupName = Lens.lens (\PreparedStatement' {workGroupName} -> workGroupName) (\s@PreparedStatement' {} a -> s {workGroupName = a} :: PreparedStatement)

-- | The name of the prepared statement.
preparedStatement_statementName :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_statementName = Lens.lens (\PreparedStatement' {statementName} -> statementName) (\s@PreparedStatement' {} a -> s {statementName = a} :: PreparedStatement)

-- | The query string for the prepared statement.
preparedStatement_queryStatement :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_queryStatement = Lens.lens (\PreparedStatement' {queryStatement} -> queryStatement) (\s@PreparedStatement' {} a -> s {queryStatement = a} :: PreparedStatement)

-- | The last modified time of the prepared statement.
preparedStatement_lastModifiedTime :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.UTCTime)
preparedStatement_lastModifiedTime = Lens.lens (\PreparedStatement' {lastModifiedTime} -> lastModifiedTime) (\s@PreparedStatement' {} a -> s {lastModifiedTime = a} :: PreparedStatement) Prelude.. Lens.mapping Core._Time

-- | The description of the prepared statement.
preparedStatement_description :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_description = Lens.lens (\PreparedStatement' {description} -> description) (\s@PreparedStatement' {} a -> s {description = a} :: PreparedStatement)

instance Core.FromJSON PreparedStatement where
  parseJSON =
    Core.withObject
      "PreparedStatement"
      ( \x ->
          PreparedStatement'
            Prelude.<$> (x Core..:? "WorkGroupName")
            Prelude.<*> (x Core..:? "StatementName")
            Prelude.<*> (x Core..:? "QueryStatement")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable PreparedStatement

instance Prelude.NFData PreparedStatement
