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
-- Module      : Amazonka.RDS.Types.SourceRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.SourceRegion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an Amazon Web Services Region name as the result of a
-- successful call to the @DescribeSourceRegions@ action.
--
-- /See:/ 'newSourceRegion' smart constructor.
data SourceRegion = SourceRegion'
  { -- | The endpoint for the source Amazon Web Services Region endpoint.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The name of the source Amazon Web Services Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The status of the source Amazon Web Services Region.
    status :: Prelude.Maybe Prelude.Text,
    -- | Whether the source Amazon Web Services Region supports replicating
    -- automated backups to the current Amazon Web Services Region.
    supportsDBInstanceAutomatedBackupsReplication :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'sourceRegion_endpoint' - The endpoint for the source Amazon Web Services Region endpoint.
--
-- 'regionName', 'sourceRegion_regionName' - The name of the source Amazon Web Services Region.
--
-- 'status', 'sourceRegion_status' - The status of the source Amazon Web Services Region.
--
-- 'supportsDBInstanceAutomatedBackupsReplication', 'sourceRegion_supportsDBInstanceAutomatedBackupsReplication' - Whether the source Amazon Web Services Region supports replicating
-- automated backups to the current Amazon Web Services Region.
newSourceRegion ::
  SourceRegion
newSourceRegion =
  SourceRegion'
    { endpoint = Prelude.Nothing,
      regionName = Prelude.Nothing,
      status = Prelude.Nothing,
      supportsDBInstanceAutomatedBackupsReplication =
        Prelude.Nothing
    }

-- | The endpoint for the source Amazon Web Services Region endpoint.
sourceRegion_endpoint :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Text)
sourceRegion_endpoint = Lens.lens (\SourceRegion' {endpoint} -> endpoint) (\s@SourceRegion' {} a -> s {endpoint = a} :: SourceRegion)

-- | The name of the source Amazon Web Services Region.
sourceRegion_regionName :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Text)
sourceRegion_regionName = Lens.lens (\SourceRegion' {regionName} -> regionName) (\s@SourceRegion' {} a -> s {regionName = a} :: SourceRegion)

-- | The status of the source Amazon Web Services Region.
sourceRegion_status :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Text)
sourceRegion_status = Lens.lens (\SourceRegion' {status} -> status) (\s@SourceRegion' {} a -> s {status = a} :: SourceRegion)

-- | Whether the source Amazon Web Services Region supports replicating
-- automated backups to the current Amazon Web Services Region.
sourceRegion_supportsDBInstanceAutomatedBackupsReplication :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Bool)
sourceRegion_supportsDBInstanceAutomatedBackupsReplication = Lens.lens (\SourceRegion' {supportsDBInstanceAutomatedBackupsReplication} -> supportsDBInstanceAutomatedBackupsReplication) (\s@SourceRegion' {} a -> s {supportsDBInstanceAutomatedBackupsReplication = a} :: SourceRegion)

instance Data.FromXML SourceRegion where
  parseXML x =
    SourceRegion'
      Prelude.<$> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "RegionName")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> ( x
                      Data..@? "SupportsDBInstanceAutomatedBackupsReplication"
                  )

instance Prelude.Hashable SourceRegion where
  hashWithSalt _salt SourceRegion' {..} =
    _salt
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` supportsDBInstanceAutomatedBackupsReplication

instance Prelude.NFData SourceRegion where
  rnf SourceRegion' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        supportsDBInstanceAutomatedBackupsReplication
