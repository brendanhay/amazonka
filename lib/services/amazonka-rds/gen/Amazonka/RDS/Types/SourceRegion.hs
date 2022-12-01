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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.SourceRegion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an Amazon Web Services Region name as the result of a
-- successful call to the @DescribeSourceRegions@ action.
--
-- /See:/ 'newSourceRegion' smart constructor.
data SourceRegion = SourceRegion'
  { -- | The status of the source Amazon Web Services Region.
    status :: Prelude.Maybe Prelude.Text,
    -- | Whether the source Amazon Web Services Region supports replicating
    -- automated backups to the current Amazon Web Services Region.
    supportsDBInstanceAutomatedBackupsReplication :: Prelude.Maybe Prelude.Bool,
    -- | The name of the source Amazon Web Services Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for the source Amazon Web Services Region endpoint.
    endpoint :: Prelude.Maybe Prelude.Text
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
-- 'status', 'sourceRegion_status' - The status of the source Amazon Web Services Region.
--
-- 'supportsDBInstanceAutomatedBackupsReplication', 'sourceRegion_supportsDBInstanceAutomatedBackupsReplication' - Whether the source Amazon Web Services Region supports replicating
-- automated backups to the current Amazon Web Services Region.
--
-- 'regionName', 'sourceRegion_regionName' - The name of the source Amazon Web Services Region.
--
-- 'endpoint', 'sourceRegion_endpoint' - The endpoint for the source Amazon Web Services Region endpoint.
newSourceRegion ::
  SourceRegion
newSourceRegion =
  SourceRegion'
    { status = Prelude.Nothing,
      supportsDBInstanceAutomatedBackupsReplication =
        Prelude.Nothing,
      regionName = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The status of the source Amazon Web Services Region.
sourceRegion_status :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Text)
sourceRegion_status = Lens.lens (\SourceRegion' {status} -> status) (\s@SourceRegion' {} a -> s {status = a} :: SourceRegion)

-- | Whether the source Amazon Web Services Region supports replicating
-- automated backups to the current Amazon Web Services Region.
sourceRegion_supportsDBInstanceAutomatedBackupsReplication :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Bool)
sourceRegion_supportsDBInstanceAutomatedBackupsReplication = Lens.lens (\SourceRegion' {supportsDBInstanceAutomatedBackupsReplication} -> supportsDBInstanceAutomatedBackupsReplication) (\s@SourceRegion' {} a -> s {supportsDBInstanceAutomatedBackupsReplication = a} :: SourceRegion)

-- | The name of the source Amazon Web Services Region.
sourceRegion_regionName :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Text)
sourceRegion_regionName = Lens.lens (\SourceRegion' {regionName} -> regionName) (\s@SourceRegion' {} a -> s {regionName = a} :: SourceRegion)

-- | The endpoint for the source Amazon Web Services Region endpoint.
sourceRegion_endpoint :: Lens.Lens' SourceRegion (Prelude.Maybe Prelude.Text)
sourceRegion_endpoint = Lens.lens (\SourceRegion' {endpoint} -> endpoint) (\s@SourceRegion' {} a -> s {endpoint = a} :: SourceRegion)

instance Core.FromXML SourceRegion where
  parseXML x =
    SourceRegion'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> ( x
                      Core..@? "SupportsDBInstanceAutomatedBackupsReplication"
                  )
      Prelude.<*> (x Core..@? "RegionName")
      Prelude.<*> (x Core..@? "Endpoint")

instance Prelude.Hashable SourceRegion where
  hashWithSalt _salt SourceRegion' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` supportsDBInstanceAutomatedBackupsReplication
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData SourceRegion where
  rnf SourceRegion' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        supportsDBInstanceAutomatedBackupsReplication
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf endpoint
