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
-- Module      : Amazonka.MachineLearning.Types.RDSDatabase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RDSDatabase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The database details of an Amazon RDS database.
--
-- /See:/ 'newRDSDatabase' smart constructor.
data RDSDatabase = RDSDatabase'
  { -- | The ID of an RDS DB instance.
    instanceIdentifier :: Prelude.Text,
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON RDSDatabase where
  parseJSON =
    Data.withObject
      "RDSDatabase"
      ( \x ->
          RDSDatabase'
            Prelude.<$> (x Data..: "InstanceIdentifier")
            Prelude.<*> (x Data..: "DatabaseName")
      )

instance Prelude.Hashable RDSDatabase where
  hashWithSalt _salt RDSDatabase' {..} =
    _salt `Prelude.hashWithSalt` instanceIdentifier
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData RDSDatabase where
  rnf RDSDatabase' {..} =
    Prelude.rnf instanceIdentifier
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToJSON RDSDatabase where
  toJSON RDSDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InstanceIdentifier" Data..= instanceIdentifier),
            Prelude.Just ("DatabaseName" Data..= databaseName)
          ]
      )
