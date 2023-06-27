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
-- Module      : Amazonka.CleanRooms.Types.GlueTableReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.GlueTableReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reference to a table within an AWS Glue data catalog.
--
-- /See:/ 'newGlueTableReference' smart constructor.
data GlueTableReference = GlueTableReference'
  { -- | The name of the AWS Glue table.
    tableName :: Prelude.Text,
    -- | The name of the database the AWS Glue table belongs to.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlueTableReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'glueTableReference_tableName' - The name of the AWS Glue table.
--
-- 'databaseName', 'glueTableReference_databaseName' - The name of the database the AWS Glue table belongs to.
newGlueTableReference ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  GlueTableReference
newGlueTableReference pTableName_ pDatabaseName_ =
  GlueTableReference'
    { tableName = pTableName_,
      databaseName = pDatabaseName_
    }

-- | The name of the AWS Glue table.
glueTableReference_tableName :: Lens.Lens' GlueTableReference Prelude.Text
glueTableReference_tableName = Lens.lens (\GlueTableReference' {tableName} -> tableName) (\s@GlueTableReference' {} a -> s {tableName = a} :: GlueTableReference)

-- | The name of the database the AWS Glue table belongs to.
glueTableReference_databaseName :: Lens.Lens' GlueTableReference Prelude.Text
glueTableReference_databaseName = Lens.lens (\GlueTableReference' {databaseName} -> databaseName) (\s@GlueTableReference' {} a -> s {databaseName = a} :: GlueTableReference)

instance Data.FromJSON GlueTableReference where
  parseJSON =
    Data.withObject
      "GlueTableReference"
      ( \x ->
          GlueTableReference'
            Prelude.<$> (x Data..: "tableName")
            Prelude.<*> (x Data..: "databaseName")
      )

instance Prelude.Hashable GlueTableReference where
  hashWithSalt _salt GlueTableReference' {..} =
    _salt
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData GlueTableReference where
  rnf GlueTableReference' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToJSON GlueTableReference where
  toJSON GlueTableReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tableName" Data..= tableName),
            Prelude.Just ("databaseName" Data..= databaseName)
          ]
      )
