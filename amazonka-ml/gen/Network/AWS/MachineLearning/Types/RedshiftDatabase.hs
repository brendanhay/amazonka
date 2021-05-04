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
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDatabase where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the database details required to connect to an Amazon Redshift
-- database.
--
-- /See:/ 'newRedshiftDatabase' smart constructor.
data RedshiftDatabase = RedshiftDatabase'
  { databaseName :: Prelude.Text,
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'redshiftDatabase_databaseName' - Undocumented member.
--
-- 'clusterIdentifier', 'redshiftDatabase_clusterIdentifier' - Undocumented member.
newRedshiftDatabase ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  RedshiftDatabase
newRedshiftDatabase
  pDatabaseName_
  pClusterIdentifier_ =
    RedshiftDatabase'
      { databaseName = pDatabaseName_,
        clusterIdentifier = pClusterIdentifier_
      }

-- | Undocumented member.
redshiftDatabase_databaseName :: Lens.Lens' RedshiftDatabase Prelude.Text
redshiftDatabase_databaseName = Lens.lens (\RedshiftDatabase' {databaseName} -> databaseName) (\s@RedshiftDatabase' {} a -> s {databaseName = a} :: RedshiftDatabase)

-- | Undocumented member.
redshiftDatabase_clusterIdentifier :: Lens.Lens' RedshiftDatabase Prelude.Text
redshiftDatabase_clusterIdentifier = Lens.lens (\RedshiftDatabase' {clusterIdentifier} -> clusterIdentifier) (\s@RedshiftDatabase' {} a -> s {clusterIdentifier = a} :: RedshiftDatabase)

instance Prelude.FromJSON RedshiftDatabase where
  parseJSON =
    Prelude.withObject
      "RedshiftDatabase"
      ( \x ->
          RedshiftDatabase'
            Prelude.<$> (x Prelude..: "DatabaseName")
            Prelude.<*> (x Prelude..: "ClusterIdentifier")
      )

instance Prelude.Hashable RedshiftDatabase

instance Prelude.NFData RedshiftDatabase

instance Prelude.ToJSON RedshiftDatabase where
  toJSON RedshiftDatabase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just
              ("ClusterIdentifier" Prelude..= clusterIdentifier)
          ]
      )
