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
-- Module      : Amazonka.KeySpaces.Types.TableSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.TableSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns the name of the specified table, the keyspace it is stored in,
-- and the unique identifier in the format of an Amazon Resource Name
-- (ARN).
--
-- /See:/ 'newTableSummary' smart constructor.
data TableSummary = TableSummary'
  { -- | The name of the keyspace that the table is stored in.
    keyspaceName :: Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Text,
    -- | The unique identifier of the table in the format of an Amazon Resource
    -- Name (ARN).
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyspaceName', 'tableSummary_keyspaceName' - The name of the keyspace that the table is stored in.
--
-- 'tableName', 'tableSummary_tableName' - The name of the table.
--
-- 'resourceArn', 'tableSummary_resourceArn' - The unique identifier of the table in the format of an Amazon Resource
-- Name (ARN).
newTableSummary ::
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  TableSummary
newTableSummary
  pKeyspaceName_
  pTableName_
  pResourceArn_ =
    TableSummary'
      { keyspaceName = pKeyspaceName_,
        tableName = pTableName_,
        resourceArn = pResourceArn_
      }

-- | The name of the keyspace that the table is stored in.
tableSummary_keyspaceName :: Lens.Lens' TableSummary Prelude.Text
tableSummary_keyspaceName = Lens.lens (\TableSummary' {keyspaceName} -> keyspaceName) (\s@TableSummary' {} a -> s {keyspaceName = a} :: TableSummary)

-- | The name of the table.
tableSummary_tableName :: Lens.Lens' TableSummary Prelude.Text
tableSummary_tableName = Lens.lens (\TableSummary' {tableName} -> tableName) (\s@TableSummary' {} a -> s {tableName = a} :: TableSummary)

-- | The unique identifier of the table in the format of an Amazon Resource
-- Name (ARN).
tableSummary_resourceArn :: Lens.Lens' TableSummary Prelude.Text
tableSummary_resourceArn = Lens.lens (\TableSummary' {resourceArn} -> resourceArn) (\s@TableSummary' {} a -> s {resourceArn = a} :: TableSummary)

instance Core.FromJSON TableSummary where
  parseJSON =
    Core.withObject
      "TableSummary"
      ( \x ->
          TableSummary'
            Prelude.<$> (x Core..: "keyspaceName")
            Prelude.<*> (x Core..: "tableName")
            Prelude.<*> (x Core..: "resourceArn")
      )

instance Prelude.Hashable TableSummary where
  hashWithSalt _salt TableSummary' {..} =
    _salt `Prelude.hashWithSalt` keyspaceName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData TableSummary where
  rnf TableSummary' {..} =
    Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf resourceArn
