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
-- Module      : Amazonka.MachineLearning.Types.RedshiftMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RedshiftMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types.RedshiftDatabase
import qualified Amazonka.Prelude as Prelude

-- | Describes the @DataSource@ details specific to Amazon Redshift.
--
-- /See:/ 'newRedshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
  { databaseUserName :: Prelude.Maybe Prelude.Text,
    redshiftDatabase :: Prelude.Maybe RedshiftDatabase,
    -- | The SQL query that is specified during CreateDataSourceFromRedshift.
    -- Returns only if @Verbose@ is true in GetDataSourceInput.
    selectSqlQuery :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseUserName', 'redshiftMetadata_databaseUserName' - Undocumented member.
--
-- 'redshiftDatabase', 'redshiftMetadata_redshiftDatabase' - Undocumented member.
--
-- 'selectSqlQuery', 'redshiftMetadata_selectSqlQuery' - The SQL query that is specified during CreateDataSourceFromRedshift.
-- Returns only if @Verbose@ is true in GetDataSourceInput.
newRedshiftMetadata ::
  RedshiftMetadata
newRedshiftMetadata =
  RedshiftMetadata'
    { databaseUserName =
        Prelude.Nothing,
      redshiftDatabase = Prelude.Nothing,
      selectSqlQuery = Prelude.Nothing
    }

-- | Undocumented member.
redshiftMetadata_databaseUserName :: Lens.Lens' RedshiftMetadata (Prelude.Maybe Prelude.Text)
redshiftMetadata_databaseUserName = Lens.lens (\RedshiftMetadata' {databaseUserName} -> databaseUserName) (\s@RedshiftMetadata' {} a -> s {databaseUserName = a} :: RedshiftMetadata)

-- | Undocumented member.
redshiftMetadata_redshiftDatabase :: Lens.Lens' RedshiftMetadata (Prelude.Maybe RedshiftDatabase)
redshiftMetadata_redshiftDatabase = Lens.lens (\RedshiftMetadata' {redshiftDatabase} -> redshiftDatabase) (\s@RedshiftMetadata' {} a -> s {redshiftDatabase = a} :: RedshiftMetadata)

-- | The SQL query that is specified during CreateDataSourceFromRedshift.
-- Returns only if @Verbose@ is true in GetDataSourceInput.
redshiftMetadata_selectSqlQuery :: Lens.Lens' RedshiftMetadata (Prelude.Maybe Prelude.Text)
redshiftMetadata_selectSqlQuery = Lens.lens (\RedshiftMetadata' {selectSqlQuery} -> selectSqlQuery) (\s@RedshiftMetadata' {} a -> s {selectSqlQuery = a} :: RedshiftMetadata)

instance Data.FromJSON RedshiftMetadata where
  parseJSON =
    Data.withObject
      "RedshiftMetadata"
      ( \x ->
          RedshiftMetadata'
            Prelude.<$> (x Data..:? "DatabaseUserName")
            Prelude.<*> (x Data..:? "RedshiftDatabase")
            Prelude.<*> (x Data..:? "SelectSqlQuery")
      )

instance Prelude.Hashable RedshiftMetadata where
  hashWithSalt _salt RedshiftMetadata' {..} =
    _salt `Prelude.hashWithSalt` databaseUserName
      `Prelude.hashWithSalt` redshiftDatabase
      `Prelude.hashWithSalt` selectSqlQuery

instance Prelude.NFData RedshiftMetadata where
  rnf RedshiftMetadata' {..} =
    Prelude.rnf databaseUserName
      `Prelude.seq` Prelude.rnf redshiftDatabase
      `Prelude.seq` Prelude.rnf selectSqlQuery
