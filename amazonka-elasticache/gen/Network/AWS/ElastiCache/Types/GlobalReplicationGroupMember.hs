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
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember where

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A member of a Global Datastore. It contains the Replication Group Id,
-- the AWS region and the role of the replication group.
--
-- /See:/ 'newGlobalReplicationGroupMember' smart constructor.
data GlobalReplicationGroupMember = GlobalReplicationGroupMember'
  { -- | The status of the membership of the replication group.
    status :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether automatic failover is enabled for the replication
    -- group.
    automaticFailover :: Prelude.Maybe AutomaticFailoverStatus,
    -- | The replication group id of the Global Datastore member.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The AWS region of the Global Datastore member.
    replicationGroupRegion :: Prelude.Maybe Prelude.Text,
    -- | Indicates the role of the replication group, primary or secondary.
    role' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GlobalReplicationGroupMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'globalReplicationGroupMember_status' - The status of the membership of the replication group.
--
-- 'automaticFailover', 'globalReplicationGroupMember_automaticFailover' - Indicates whether automatic failover is enabled for the replication
-- group.
--
-- 'replicationGroupId', 'globalReplicationGroupMember_replicationGroupId' - The replication group id of the Global Datastore member.
--
-- 'replicationGroupRegion', 'globalReplicationGroupMember_replicationGroupRegion' - The AWS region of the Global Datastore member.
--
-- 'role'', 'globalReplicationGroupMember_role' - Indicates the role of the replication group, primary or secondary.
newGlobalReplicationGroupMember ::
  GlobalReplicationGroupMember
newGlobalReplicationGroupMember =
  GlobalReplicationGroupMember'
    { status =
        Prelude.Nothing,
      automaticFailover = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      replicationGroupRegion = Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | The status of the membership of the replication group.
globalReplicationGroupMember_status :: Lens.Lens' GlobalReplicationGroupMember (Prelude.Maybe Prelude.Text)
globalReplicationGroupMember_status = Lens.lens (\GlobalReplicationGroupMember' {status} -> status) (\s@GlobalReplicationGroupMember' {} a -> s {status = a} :: GlobalReplicationGroupMember)

-- | Indicates whether automatic failover is enabled for the replication
-- group.
globalReplicationGroupMember_automaticFailover :: Lens.Lens' GlobalReplicationGroupMember (Prelude.Maybe AutomaticFailoverStatus)
globalReplicationGroupMember_automaticFailover = Lens.lens (\GlobalReplicationGroupMember' {automaticFailover} -> automaticFailover) (\s@GlobalReplicationGroupMember' {} a -> s {automaticFailover = a} :: GlobalReplicationGroupMember)

-- | The replication group id of the Global Datastore member.
globalReplicationGroupMember_replicationGroupId :: Lens.Lens' GlobalReplicationGroupMember (Prelude.Maybe Prelude.Text)
globalReplicationGroupMember_replicationGroupId = Lens.lens (\GlobalReplicationGroupMember' {replicationGroupId} -> replicationGroupId) (\s@GlobalReplicationGroupMember' {} a -> s {replicationGroupId = a} :: GlobalReplicationGroupMember)

-- | The AWS region of the Global Datastore member.
globalReplicationGroupMember_replicationGroupRegion :: Lens.Lens' GlobalReplicationGroupMember (Prelude.Maybe Prelude.Text)
globalReplicationGroupMember_replicationGroupRegion = Lens.lens (\GlobalReplicationGroupMember' {replicationGroupRegion} -> replicationGroupRegion) (\s@GlobalReplicationGroupMember' {} a -> s {replicationGroupRegion = a} :: GlobalReplicationGroupMember)

-- | Indicates the role of the replication group, primary or secondary.
globalReplicationGroupMember_role :: Lens.Lens' GlobalReplicationGroupMember (Prelude.Maybe Prelude.Text)
globalReplicationGroupMember_role = Lens.lens (\GlobalReplicationGroupMember' {role'} -> role') (\s@GlobalReplicationGroupMember' {} a -> s {role' = a} :: GlobalReplicationGroupMember)

instance Prelude.FromXML GlobalReplicationGroupMember where
  parseXML x =
    GlobalReplicationGroupMember'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "AutomaticFailover")
      Prelude.<*> (x Prelude..@? "ReplicationGroupId")
      Prelude.<*> (x Prelude..@? "ReplicationGroupRegion")
      Prelude.<*> (x Prelude..@? "Role")

instance
  Prelude.Hashable
    GlobalReplicationGroupMember

instance Prelude.NFData GlobalReplicationGroupMember
