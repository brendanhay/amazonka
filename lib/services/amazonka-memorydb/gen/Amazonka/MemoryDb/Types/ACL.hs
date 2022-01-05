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
-- Module      : Amazonka.MemoryDb.Types.ACL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ACL where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types.ACLPendingChanges
import qualified Amazonka.Prelude as Prelude

-- | An Access Control List. You can authenticate users with Access Contol
-- Lists. ACLs enable you to control cluster access by grouping users.
-- These Access control lists are designed as a way to organize access to
-- clusters.
--
-- /See:/ 'newACL' smart constructor.
data ACL = ACL'
  { -- | Indicates ACL status. Can be \"creating\", \"active\", \"modifying\",
    -- \"deleting\".
    status :: Prelude.Maybe Prelude.Text,
    -- | The list of user names that belong to the ACL.
    userNames :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the ACL
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Access Control List
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of updates being applied to the ACL.
    pendingChanges :: Prelude.Maybe ACLPendingChanges,
    -- | The minimum engine version supported for the ACL
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of clusters associated with the ACL.
    clusters :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'acl_status' - Indicates ACL status. Can be \"creating\", \"active\", \"modifying\",
-- \"deleting\".
--
-- 'userNames', 'acl_userNames' - The list of user names that belong to the ACL.
--
-- 'arn', 'acl_arn' - The Amazon Resource Name (ARN) of the ACL
--
-- 'name', 'acl_name' - The name of the Access Control List
--
-- 'pendingChanges', 'acl_pendingChanges' - A list of updates being applied to the ACL.
--
-- 'minimumEngineVersion', 'acl_minimumEngineVersion' - The minimum engine version supported for the ACL
--
-- 'clusters', 'acl_clusters' - A list of clusters associated with the ACL.
newACL ::
  ACL
newACL =
  ACL'
    { status = Prelude.Nothing,
      userNames = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      pendingChanges = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing,
      clusters = Prelude.Nothing
    }

-- | Indicates ACL status. Can be \"creating\", \"active\", \"modifying\",
-- \"deleting\".
acl_status :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_status = Lens.lens (\ACL' {status} -> status) (\s@ACL' {} a -> s {status = a} :: ACL)

-- | The list of user names that belong to the ACL.
acl_userNames :: Lens.Lens' ACL (Prelude.Maybe [Prelude.Text])
acl_userNames = Lens.lens (\ACL' {userNames} -> userNames) (\s@ACL' {} a -> s {userNames = a} :: ACL) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the ACL
acl_arn :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_arn = Lens.lens (\ACL' {arn} -> arn) (\s@ACL' {} a -> s {arn = a} :: ACL)

-- | The name of the Access Control List
acl_name :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_name = Lens.lens (\ACL' {name} -> name) (\s@ACL' {} a -> s {name = a} :: ACL)

-- | A list of updates being applied to the ACL.
acl_pendingChanges :: Lens.Lens' ACL (Prelude.Maybe ACLPendingChanges)
acl_pendingChanges = Lens.lens (\ACL' {pendingChanges} -> pendingChanges) (\s@ACL' {} a -> s {pendingChanges = a} :: ACL)

-- | The minimum engine version supported for the ACL
acl_minimumEngineVersion :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_minimumEngineVersion = Lens.lens (\ACL' {minimumEngineVersion} -> minimumEngineVersion) (\s@ACL' {} a -> s {minimumEngineVersion = a} :: ACL)

-- | A list of clusters associated with the ACL.
acl_clusters :: Lens.Lens' ACL (Prelude.Maybe [Prelude.Text])
acl_clusters = Lens.lens (\ACL' {clusters} -> clusters) (\s@ACL' {} a -> s {clusters = a} :: ACL) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ACL where
  parseJSON =
    Core.withObject
      "ACL"
      ( \x ->
          ACL'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "UserNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "PendingChanges")
            Prelude.<*> (x Core..:? "MinimumEngineVersion")
            Prelude.<*> (x Core..:? "Clusters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ACL where
  hashWithSalt _salt ACL' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userNames
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pendingChanges
      `Prelude.hashWithSalt` minimumEngineVersion
      `Prelude.hashWithSalt` clusters

instance Prelude.NFData ACL where
  rnf ACL' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf userNames
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pendingChanges
      `Prelude.seq` Prelude.rnf minimumEngineVersion
      `Prelude.seq` Prelude.rnf clusters
