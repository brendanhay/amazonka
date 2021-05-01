{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MachineLearning.Types.RedshiftMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftMetadata where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import qualified Network.AWS.Prelude as Prelude

-- | Describes the @DataSource@ details specific to Amazon Redshift.
--
-- /See:/ 'newRedshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
  { -- | The SQL query that is specified during CreateDataSourceFromRedshift.
    -- Returns only if @Verbose@ is true in GetDataSourceInput.
    selectSqlQuery :: Prelude.Maybe Prelude.Text,
    redshiftDatabase :: Prelude.Maybe RedshiftDatabase,
    databaseUserName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RedshiftMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectSqlQuery', 'redshiftMetadata_selectSqlQuery' - The SQL query that is specified during CreateDataSourceFromRedshift.
-- Returns only if @Verbose@ is true in GetDataSourceInput.
--
-- 'redshiftDatabase', 'redshiftMetadata_redshiftDatabase' - Undocumented member.
--
-- 'databaseUserName', 'redshiftMetadata_databaseUserName' - Undocumented member.
newRedshiftMetadata ::
  RedshiftMetadata
newRedshiftMetadata =
  RedshiftMetadata'
    { selectSqlQuery = Prelude.Nothing,
      redshiftDatabase = Prelude.Nothing,
      databaseUserName = Prelude.Nothing
    }

-- | The SQL query that is specified during CreateDataSourceFromRedshift.
-- Returns only if @Verbose@ is true in GetDataSourceInput.
redshiftMetadata_selectSqlQuery :: Lens.Lens' RedshiftMetadata (Prelude.Maybe Prelude.Text)
redshiftMetadata_selectSqlQuery = Lens.lens (\RedshiftMetadata' {selectSqlQuery} -> selectSqlQuery) (\s@RedshiftMetadata' {} a -> s {selectSqlQuery = a} :: RedshiftMetadata)

-- | Undocumented member.
redshiftMetadata_redshiftDatabase :: Lens.Lens' RedshiftMetadata (Prelude.Maybe RedshiftDatabase)
redshiftMetadata_redshiftDatabase = Lens.lens (\RedshiftMetadata' {redshiftDatabase} -> redshiftDatabase) (\s@RedshiftMetadata' {} a -> s {redshiftDatabase = a} :: RedshiftMetadata)

-- | Undocumented member.
redshiftMetadata_databaseUserName :: Lens.Lens' RedshiftMetadata (Prelude.Maybe Prelude.Text)
redshiftMetadata_databaseUserName = Lens.lens (\RedshiftMetadata' {databaseUserName} -> databaseUserName) (\s@RedshiftMetadata' {} a -> s {databaseUserName = a} :: RedshiftMetadata)

instance Prelude.FromJSON RedshiftMetadata where
  parseJSON =
    Prelude.withObject
      "RedshiftMetadata"
      ( \x ->
          RedshiftMetadata'
            Prelude.<$> (x Prelude..:? "SelectSqlQuery")
            Prelude.<*> (x Prelude..:? "RedshiftDatabase")
            Prelude.<*> (x Prelude..:? "DatabaseUserName")
      )

instance Prelude.Hashable RedshiftMetadata

instance Prelude.NFData RedshiftMetadata
