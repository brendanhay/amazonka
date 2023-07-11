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
-- Module      : Amazonka.Redshift.Types.ClusterSecurityGroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterSecurityGroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a cluster security group.
--
-- /See:/ 'newClusterSecurityGroupMembership' smart constructor.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership'
  { -- | The name of the cluster security group.
    clusterSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the cluster security group.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterSecurityGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSecurityGroupName', 'clusterSecurityGroupMembership_clusterSecurityGroupName' - The name of the cluster security group.
--
-- 'status', 'clusterSecurityGroupMembership_status' - The status of the cluster security group.
newClusterSecurityGroupMembership ::
  ClusterSecurityGroupMembership
newClusterSecurityGroupMembership =
  ClusterSecurityGroupMembership'
    { clusterSecurityGroupName =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the cluster security group.
clusterSecurityGroupMembership_clusterSecurityGroupName :: Lens.Lens' ClusterSecurityGroupMembership (Prelude.Maybe Prelude.Text)
clusterSecurityGroupMembership_clusterSecurityGroupName = Lens.lens (\ClusterSecurityGroupMembership' {clusterSecurityGroupName} -> clusterSecurityGroupName) (\s@ClusterSecurityGroupMembership' {} a -> s {clusterSecurityGroupName = a} :: ClusterSecurityGroupMembership)

-- | The status of the cluster security group.
clusterSecurityGroupMembership_status :: Lens.Lens' ClusterSecurityGroupMembership (Prelude.Maybe Prelude.Text)
clusterSecurityGroupMembership_status = Lens.lens (\ClusterSecurityGroupMembership' {status} -> status) (\s@ClusterSecurityGroupMembership' {} a -> s {status = a} :: ClusterSecurityGroupMembership)

instance Data.FromXML ClusterSecurityGroupMembership where
  parseXML x =
    ClusterSecurityGroupMembership'
      Prelude.<$> (x Data..@? "ClusterSecurityGroupName")
      Prelude.<*> (x Data..@? "Status")

instance
  Prelude.Hashable
    ClusterSecurityGroupMembership
  where
  hashWithSalt
    _salt
    ClusterSecurityGroupMembership' {..} =
      _salt
        `Prelude.hashWithSalt` clusterSecurityGroupName
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ClusterSecurityGroupMembership
  where
  rnf ClusterSecurityGroupMembership' {..} =
    Prelude.rnf clusterSecurityGroupName
      `Prelude.seq` Prelude.rnf status
