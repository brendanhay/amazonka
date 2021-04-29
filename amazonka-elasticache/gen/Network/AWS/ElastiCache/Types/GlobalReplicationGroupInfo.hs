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
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The name of the Global Datastore and role of this replication group in
-- the Global Datastore.
--
-- /See:/ 'newGlobalReplicationGroupInfo' smart constructor.
data GlobalReplicationGroupInfo = GlobalReplicationGroupInfo'
  { -- | The role of the replication group in a Global Datastore. Can be primary
    -- or secondary.
    globalReplicationGroupMemberRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GlobalReplicationGroupInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupMemberRole', 'globalReplicationGroupInfo_globalReplicationGroupMemberRole' - The role of the replication group in a Global Datastore. Can be primary
-- or secondary.
--
-- 'globalReplicationGroupId', 'globalReplicationGroupInfo_globalReplicationGroupId' - The name of the Global Datastore
newGlobalReplicationGroupInfo ::
  GlobalReplicationGroupInfo
newGlobalReplicationGroupInfo =
  GlobalReplicationGroupInfo'
    { globalReplicationGroupMemberRole =
        Prelude.Nothing,
      globalReplicationGroupId = Prelude.Nothing
    }

-- | The role of the replication group in a Global Datastore. Can be primary
-- or secondary.
globalReplicationGroupInfo_globalReplicationGroupMemberRole :: Lens.Lens' GlobalReplicationGroupInfo (Prelude.Maybe Prelude.Text)
globalReplicationGroupInfo_globalReplicationGroupMemberRole = Lens.lens (\GlobalReplicationGroupInfo' {globalReplicationGroupMemberRole} -> globalReplicationGroupMemberRole) (\s@GlobalReplicationGroupInfo' {} a -> s {globalReplicationGroupMemberRole = a} :: GlobalReplicationGroupInfo)

-- | The name of the Global Datastore
globalReplicationGroupInfo_globalReplicationGroupId :: Lens.Lens' GlobalReplicationGroupInfo (Prelude.Maybe Prelude.Text)
globalReplicationGroupInfo_globalReplicationGroupId = Lens.lens (\GlobalReplicationGroupInfo' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@GlobalReplicationGroupInfo' {} a -> s {globalReplicationGroupId = a} :: GlobalReplicationGroupInfo)

instance Prelude.FromXML GlobalReplicationGroupInfo where
  parseXML x =
    GlobalReplicationGroupInfo'
      Prelude.<$> (x Prelude..@? "GlobalReplicationGroupMemberRole")
      Prelude.<*> (x Prelude..@? "GlobalReplicationGroupId")

instance Prelude.Hashable GlobalReplicationGroupInfo

instance Prelude.NFData GlobalReplicationGroupInfo
