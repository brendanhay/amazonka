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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RedshiftMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MachineLearning.Types.RedshiftDatabase
import qualified Amazonka.Prelude as Prelude

-- | Describes the @DataSource@ details specific to Amazon Redshift.
--
-- /See:/ 'newRedshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
  { databaseUserName :: Prelude.Maybe Prelude.Text,
    -- | The SQL query that is specified during CreateDataSourceFromRedshift.
    -- Returns only if @Verbose@ is true in GetDataSourceInput.
    selectSqlQuery :: Prelude.Maybe Prelude.Text,
    redshiftDatabase :: Prelude.Maybe RedshiftDatabase
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
-- 'selectSqlQuery', 'redshiftMetadata_selectSqlQuery' - The SQL query that is specified during CreateDataSourceFromRedshift.
-- Returns only if @Verbose@ is true in GetDataSourceInput.
--
-- 'redshiftDatabase', 'redshiftMetadata_redshiftDatabase' - Undocumented member.
newRedshiftMetadata ::
  RedshiftMetadata
newRedshiftMetadata =
  RedshiftMetadata'
    { databaseUserName =
        Prelude.Nothing,
      selectSqlQuery = Prelude.Nothing,
      redshiftDatabase = Prelude.Nothing
    }

-- | Undocumented member.
redshiftMetadata_databaseUserName :: Lens.Lens' RedshiftMetadata (Prelude.Maybe Prelude.Text)
redshiftMetadata_databaseUserName = Lens.lens (\RedshiftMetadata' {databaseUserName} -> databaseUserName) (\s@RedshiftMetadata' {} a -> s {databaseUserName = a} :: RedshiftMetadata)

-- | The SQL query that is specified during CreateDataSourceFromRedshift.
-- Returns only if @Verbose@ is true in GetDataSourceInput.
redshiftMetadata_selectSqlQuery :: Lens.Lens' RedshiftMetadata (Prelude.Maybe Prelude.Text)
redshiftMetadata_selectSqlQuery = Lens.lens (\RedshiftMetadata' {selectSqlQuery} -> selectSqlQuery) (\s@RedshiftMetadata' {} a -> s {selectSqlQuery = a} :: RedshiftMetadata)

-- | Undocumented member.
redshiftMetadata_redshiftDatabase :: Lens.Lens' RedshiftMetadata (Prelude.Maybe RedshiftDatabase)
redshiftMetadata_redshiftDatabase = Lens.lens (\RedshiftMetadata' {redshiftDatabase} -> redshiftDatabase) (\s@RedshiftMetadata' {} a -> s {redshiftDatabase = a} :: RedshiftMetadata)

instance Core.FromJSON RedshiftMetadata where
  parseJSON =
    Core.withObject
      "RedshiftMetadata"
      ( \x ->
          RedshiftMetadata'
            Prelude.<$> (x Core..:? "DatabaseUserName")
            Prelude.<*> (x Core..:? "SelectSqlQuery")
            Prelude.<*> (x Core..:? "RedshiftDatabase")
      )

instance Prelude.Hashable RedshiftMetadata where
  hashWithSalt _salt RedshiftMetadata' {..} =
    _salt `Prelude.hashWithSalt` databaseUserName
      `Prelude.hashWithSalt` selectSqlQuery
      `Prelude.hashWithSalt` redshiftDatabase

instance Prelude.NFData RedshiftMetadata where
  rnf RedshiftMetadata' {..} =
    Prelude.rnf databaseUserName
      `Prelude.seq` Prelude.rnf selectSqlQuery
      `Prelude.seq` Prelude.rnf redshiftDatabase
