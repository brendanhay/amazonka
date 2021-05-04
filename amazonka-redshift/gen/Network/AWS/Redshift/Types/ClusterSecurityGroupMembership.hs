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
-- Module      : Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSecurityGroupMembership where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes a cluster security group.
--
-- /See:/ 'newClusterSecurityGroupMembership' smart constructor.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership'
  { -- | The status of the cluster security group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster security group.
    clusterSecurityGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClusterSecurityGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'clusterSecurityGroupMembership_status' - The status of the cluster security group.
--
-- 'clusterSecurityGroupName', 'clusterSecurityGroupMembership_clusterSecurityGroupName' - The name of the cluster security group.
newClusterSecurityGroupMembership ::
  ClusterSecurityGroupMembership
newClusterSecurityGroupMembership =
  ClusterSecurityGroupMembership'
    { status =
        Prelude.Nothing,
      clusterSecurityGroupName = Prelude.Nothing
    }

-- | The status of the cluster security group.
clusterSecurityGroupMembership_status :: Lens.Lens' ClusterSecurityGroupMembership (Prelude.Maybe Prelude.Text)
clusterSecurityGroupMembership_status = Lens.lens (\ClusterSecurityGroupMembership' {status} -> status) (\s@ClusterSecurityGroupMembership' {} a -> s {status = a} :: ClusterSecurityGroupMembership)

-- | The name of the cluster security group.
clusterSecurityGroupMembership_clusterSecurityGroupName :: Lens.Lens' ClusterSecurityGroupMembership (Prelude.Maybe Prelude.Text)
clusterSecurityGroupMembership_clusterSecurityGroupName = Lens.lens (\ClusterSecurityGroupMembership' {clusterSecurityGroupName} -> clusterSecurityGroupName) (\s@ClusterSecurityGroupMembership' {} a -> s {clusterSecurityGroupName = a} :: ClusterSecurityGroupMembership)

instance
  Prelude.FromXML
    ClusterSecurityGroupMembership
  where
  parseXML x =
    ClusterSecurityGroupMembership'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "ClusterSecurityGroupName")

instance
  Prelude.Hashable
    ClusterSecurityGroupMembership

instance
  Prelude.NFData
    ClusterSecurityGroupMembership
