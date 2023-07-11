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
-- Module      : Amazonka.DMS.Types.CollectorHealthCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.CollectorHealthCheck where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.CollectorStatus
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the last Fleet Advisor collector health check.
--
-- /See:/ 'newCollectorHealthCheck' smart constructor.
data CollectorHealthCheck = CollectorHealthCheck'
  { -- | The status of the Fleet Advisor collector.
    collectorStatus :: Prelude.Maybe CollectorStatus,
    -- | Whether the local collector can access its Amazon S3 bucket.
    localCollectorS3Access :: Prelude.Maybe Prelude.Bool,
    -- | Whether the role that you provided when creating the Fleet Advisor
    -- collector has sufficient permissions to access the Fleet Advisor web
    -- collector.
    webCollectorGrantedRoleBasedAccess :: Prelude.Maybe Prelude.Bool,
    -- | Whether the web collector can access its Amazon S3 bucket.
    webCollectorS3Access :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectorHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectorStatus', 'collectorHealthCheck_collectorStatus' - The status of the Fleet Advisor collector.
--
-- 'localCollectorS3Access', 'collectorHealthCheck_localCollectorS3Access' - Whether the local collector can access its Amazon S3 bucket.
--
-- 'webCollectorGrantedRoleBasedAccess', 'collectorHealthCheck_webCollectorGrantedRoleBasedAccess' - Whether the role that you provided when creating the Fleet Advisor
-- collector has sufficient permissions to access the Fleet Advisor web
-- collector.
--
-- 'webCollectorS3Access', 'collectorHealthCheck_webCollectorS3Access' - Whether the web collector can access its Amazon S3 bucket.
newCollectorHealthCheck ::
  CollectorHealthCheck
newCollectorHealthCheck =
  CollectorHealthCheck'
    { collectorStatus =
        Prelude.Nothing,
      localCollectorS3Access = Prelude.Nothing,
      webCollectorGrantedRoleBasedAccess = Prelude.Nothing,
      webCollectorS3Access = Prelude.Nothing
    }

-- | The status of the Fleet Advisor collector.
collectorHealthCheck_collectorStatus :: Lens.Lens' CollectorHealthCheck (Prelude.Maybe CollectorStatus)
collectorHealthCheck_collectorStatus = Lens.lens (\CollectorHealthCheck' {collectorStatus} -> collectorStatus) (\s@CollectorHealthCheck' {} a -> s {collectorStatus = a} :: CollectorHealthCheck)

-- | Whether the local collector can access its Amazon S3 bucket.
collectorHealthCheck_localCollectorS3Access :: Lens.Lens' CollectorHealthCheck (Prelude.Maybe Prelude.Bool)
collectorHealthCheck_localCollectorS3Access = Lens.lens (\CollectorHealthCheck' {localCollectorS3Access} -> localCollectorS3Access) (\s@CollectorHealthCheck' {} a -> s {localCollectorS3Access = a} :: CollectorHealthCheck)

-- | Whether the role that you provided when creating the Fleet Advisor
-- collector has sufficient permissions to access the Fleet Advisor web
-- collector.
collectorHealthCheck_webCollectorGrantedRoleBasedAccess :: Lens.Lens' CollectorHealthCheck (Prelude.Maybe Prelude.Bool)
collectorHealthCheck_webCollectorGrantedRoleBasedAccess = Lens.lens (\CollectorHealthCheck' {webCollectorGrantedRoleBasedAccess} -> webCollectorGrantedRoleBasedAccess) (\s@CollectorHealthCheck' {} a -> s {webCollectorGrantedRoleBasedAccess = a} :: CollectorHealthCheck)

-- | Whether the web collector can access its Amazon S3 bucket.
collectorHealthCheck_webCollectorS3Access :: Lens.Lens' CollectorHealthCheck (Prelude.Maybe Prelude.Bool)
collectorHealthCheck_webCollectorS3Access = Lens.lens (\CollectorHealthCheck' {webCollectorS3Access} -> webCollectorS3Access) (\s@CollectorHealthCheck' {} a -> s {webCollectorS3Access = a} :: CollectorHealthCheck)

instance Data.FromJSON CollectorHealthCheck where
  parseJSON =
    Data.withObject
      "CollectorHealthCheck"
      ( \x ->
          CollectorHealthCheck'
            Prelude.<$> (x Data..:? "CollectorStatus")
            Prelude.<*> (x Data..:? "LocalCollectorS3Access")
            Prelude.<*> (x Data..:? "WebCollectorGrantedRoleBasedAccess")
            Prelude.<*> (x Data..:? "WebCollectorS3Access")
      )

instance Prelude.Hashable CollectorHealthCheck where
  hashWithSalt _salt CollectorHealthCheck' {..} =
    _salt
      `Prelude.hashWithSalt` collectorStatus
      `Prelude.hashWithSalt` localCollectorS3Access
      `Prelude.hashWithSalt` webCollectorGrantedRoleBasedAccess
      `Prelude.hashWithSalt` webCollectorS3Access

instance Prelude.NFData CollectorHealthCheck where
  rnf CollectorHealthCheck' {..} =
    Prelude.rnf collectorStatus
      `Prelude.seq` Prelude.rnf localCollectorS3Access
      `Prelude.seq` Prelude.rnf webCollectorGrantedRoleBasedAccess
      `Prelude.seq` Prelude.rnf webCollectorS3Access
