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
-- Module      : Network.AWS.RDS.Types.SourceRegion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.SourceRegion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains an AWS Region name as the result of a successful call to the
-- @DescribeSourceRegions@ action.
--
-- /See:/ 'newSourceRegion' smart constructor.
data SourceRegion = SourceRegion'
  { -- | The name of the source AWS Region.
    regionName :: Core.Maybe Core.Text,
    -- | The status of the source AWS Region.
    status :: Core.Maybe Core.Text,
    -- | Whether the source AWS Region supports replicating automated backups to
    -- the current AWS Region.
    supportsDBInstanceAutomatedBackupsReplication :: Core.Maybe Core.Bool,
    -- | The endpoint for the source AWS Region endpoint.
    endpoint :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SourceRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'sourceRegion_regionName' - The name of the source AWS Region.
--
-- 'status', 'sourceRegion_status' - The status of the source AWS Region.
--
-- 'supportsDBInstanceAutomatedBackupsReplication', 'sourceRegion_supportsDBInstanceAutomatedBackupsReplication' - Whether the source AWS Region supports replicating automated backups to
-- the current AWS Region.
--
-- 'endpoint', 'sourceRegion_endpoint' - The endpoint for the source AWS Region endpoint.
newSourceRegion ::
  SourceRegion
newSourceRegion =
  SourceRegion'
    { regionName = Core.Nothing,
      status = Core.Nothing,
      supportsDBInstanceAutomatedBackupsReplication =
        Core.Nothing,
      endpoint = Core.Nothing
    }

-- | The name of the source AWS Region.
sourceRegion_regionName :: Lens.Lens' SourceRegion (Core.Maybe Core.Text)
sourceRegion_regionName = Lens.lens (\SourceRegion' {regionName} -> regionName) (\s@SourceRegion' {} a -> s {regionName = a} :: SourceRegion)

-- | The status of the source AWS Region.
sourceRegion_status :: Lens.Lens' SourceRegion (Core.Maybe Core.Text)
sourceRegion_status = Lens.lens (\SourceRegion' {status} -> status) (\s@SourceRegion' {} a -> s {status = a} :: SourceRegion)

-- | Whether the source AWS Region supports replicating automated backups to
-- the current AWS Region.
sourceRegion_supportsDBInstanceAutomatedBackupsReplication :: Lens.Lens' SourceRegion (Core.Maybe Core.Bool)
sourceRegion_supportsDBInstanceAutomatedBackupsReplication = Lens.lens (\SourceRegion' {supportsDBInstanceAutomatedBackupsReplication} -> supportsDBInstanceAutomatedBackupsReplication) (\s@SourceRegion' {} a -> s {supportsDBInstanceAutomatedBackupsReplication = a} :: SourceRegion)

-- | The endpoint for the source AWS Region endpoint.
sourceRegion_endpoint :: Lens.Lens' SourceRegion (Core.Maybe Core.Text)
sourceRegion_endpoint = Lens.lens (\SourceRegion' {endpoint} -> endpoint) (\s@SourceRegion' {} a -> s {endpoint = a} :: SourceRegion)

instance Core.FromXML SourceRegion where
  parseXML x =
    SourceRegion'
      Core.<$> (x Core..@? "RegionName")
      Core.<*> (x Core..@? "Status")
      Core.<*> ( x
                   Core..@? "SupportsDBInstanceAutomatedBackupsReplication"
               )
      Core.<*> (x Core..@? "Endpoint")

instance Core.Hashable SourceRegion

instance Core.NFData SourceRegion
