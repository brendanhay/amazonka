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
-- Module      : Network.AWS.MachineLearning.Types.RDSDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDatabase where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The database details of an Amazon RDS database.
--
-- /See:/ 'newRDSDatabase' smart constructor.
data RDSDatabase = RDSDatabase'
  { -- | The ID of an RDS DB instance.
    instanceIdentifier :: Prelude.Text,
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RDSDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIdentifier', 'rDSDatabase_instanceIdentifier' - The ID of an RDS DB instance.
--
-- 'databaseName', 'rDSDatabase_databaseName' - Undocumented member.
newRDSDatabase ::
  -- | 'instanceIdentifier'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  RDSDatabase
newRDSDatabase pInstanceIdentifier_ pDatabaseName_ =
  RDSDatabase'
    { instanceIdentifier =
        pInstanceIdentifier_,
      databaseName = pDatabaseName_
    }

-- | The ID of an RDS DB instance.
rDSDatabase_instanceIdentifier :: Lens.Lens' RDSDatabase Prelude.Text
rDSDatabase_instanceIdentifier = Lens.lens (\RDSDatabase' {instanceIdentifier} -> instanceIdentifier) (\s@RDSDatabase' {} a -> s {instanceIdentifier = a} :: RDSDatabase)

-- | Undocumented member.
rDSDatabase_databaseName :: Lens.Lens' RDSDatabase Prelude.Text
rDSDatabase_databaseName = Lens.lens (\RDSDatabase' {databaseName} -> databaseName) (\s@RDSDatabase' {} a -> s {databaseName = a} :: RDSDatabase)

instance Prelude.FromJSON RDSDatabase where
  parseJSON =
    Prelude.withObject
      "RDSDatabase"
      ( \x ->
          RDSDatabase'
            Prelude.<$> (x Prelude..: "InstanceIdentifier")
            Prelude.<*> (x Prelude..: "DatabaseName")
      )

instance Prelude.Hashable RDSDatabase

instance Prelude.NFData RDSDatabase

instance Prelude.ToJSON RDSDatabase where
  toJSON RDSDatabase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InstanceIdentifier" Prelude..= instanceIdentifier),
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName)
          ]
      )
