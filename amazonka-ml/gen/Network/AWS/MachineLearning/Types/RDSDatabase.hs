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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The database details of an Amazon RDS database.
--
-- /See:/ 'newRDSDatabase' smart constructor.
data RDSDatabase = RDSDatabase'
  { -- | The ID of an RDS DB instance.
    instanceIdentifier :: Core.Text,
    databaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'databaseName'
  Core.Text ->
  RDSDatabase
newRDSDatabase pInstanceIdentifier_ pDatabaseName_ =
  RDSDatabase'
    { instanceIdentifier =
        pInstanceIdentifier_,
      databaseName = pDatabaseName_
    }

-- | The ID of an RDS DB instance.
rDSDatabase_instanceIdentifier :: Lens.Lens' RDSDatabase Core.Text
rDSDatabase_instanceIdentifier = Lens.lens (\RDSDatabase' {instanceIdentifier} -> instanceIdentifier) (\s@RDSDatabase' {} a -> s {instanceIdentifier = a} :: RDSDatabase)

-- | Undocumented member.
rDSDatabase_databaseName :: Lens.Lens' RDSDatabase Core.Text
rDSDatabase_databaseName = Lens.lens (\RDSDatabase' {databaseName} -> databaseName) (\s@RDSDatabase' {} a -> s {databaseName = a} :: RDSDatabase)

instance Core.FromJSON RDSDatabase where
  parseJSON =
    Core.withObject
      "RDSDatabase"
      ( \x ->
          RDSDatabase'
            Core.<$> (x Core..: "InstanceIdentifier")
            Core.<*> (x Core..: "DatabaseName")
      )

instance Core.Hashable RDSDatabase

instance Core.NFData RDSDatabase

instance Core.ToJSON RDSDatabase where
  toJSON RDSDatabase' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("InstanceIdentifier" Core..= instanceIdentifier),
            Core.Just ("DatabaseName" Core..= databaseName)
          ]
      )
