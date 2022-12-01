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
-- Module      : Amazonka.QuickSight.Types.RdsParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RdsParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The parameters for Amazon RDS.
--
-- /See:/ 'newRdsParameters' smart constructor.
data RdsParameters = RdsParameters'
  { -- | Instance ID.
    instanceId :: Prelude.Text,
    -- | Database.
    database :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'rdsParameters_instanceId' - Instance ID.
--
-- 'database', 'rdsParameters_database' - Database.
newRdsParameters ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  RdsParameters
newRdsParameters pInstanceId_ pDatabase_ =
  RdsParameters'
    { instanceId = pInstanceId_,
      database = pDatabase_
    }

-- | Instance ID.
rdsParameters_instanceId :: Lens.Lens' RdsParameters Prelude.Text
rdsParameters_instanceId = Lens.lens (\RdsParameters' {instanceId} -> instanceId) (\s@RdsParameters' {} a -> s {instanceId = a} :: RdsParameters)

-- | Database.
rdsParameters_database :: Lens.Lens' RdsParameters Prelude.Text
rdsParameters_database = Lens.lens (\RdsParameters' {database} -> database) (\s@RdsParameters' {} a -> s {database = a} :: RdsParameters)

instance Core.FromJSON RdsParameters where
  parseJSON =
    Core.withObject
      "RdsParameters"
      ( \x ->
          RdsParameters'
            Prelude.<$> (x Core..: "InstanceId")
            Prelude.<*> (x Core..: "Database")
      )

instance Prelude.Hashable RdsParameters where
  hashWithSalt _salt RdsParameters' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` database

instance Prelude.NFData RdsParameters where
  rnf RdsParameters' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf database

instance Core.ToJSON RdsParameters where
  toJSON RdsParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Core..= instanceId),
            Prelude.Just ("Database" Core..= database)
          ]
      )
