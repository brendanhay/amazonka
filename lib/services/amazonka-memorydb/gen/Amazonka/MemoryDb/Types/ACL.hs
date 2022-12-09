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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ACL where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.ACLPendingChanges
import qualified Amazonka.Prelude as Prelude

-- | An Access Control List. You can authenticate users with Access Contol
-- Lists. ACLs enable you to control cluster access by grouping users.
-- These Access control lists are designed as a way to organize access to
-- clusters.
--
-- /See:/ 'newACL' smart constructor.
data ACL = ACL'
  { -- | The Amazon Resource Name (ARN) of the ACL
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of clusters associated with the ACL.
    clusters :: Prelude.Maybe [Prelude.Text],
    -- | The minimum engine version supported for the ACL
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the Access Control List
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of updates being applied to the ACL.
    pendingChanges :: Prelude.Maybe ACLPendingChanges,
    -- | Indicates ACL status. Can be \"creating\", \"active\", \"modifying\",
    -- \"deleting\".
    status :: Prelude.Maybe Prelude.Text,
    -- | The list of user names that belong to the ACL.
    userNames :: Prelude.Maybe [Prelude.Text]
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
-- 'arn', 'acl_arn' - The Amazon Resource Name (ARN) of the ACL
--
-- 'clusters', 'acl_clusters' - A list of clusters associated with the ACL.
--
-- 'minimumEngineVersion', 'acl_minimumEngineVersion' - The minimum engine version supported for the ACL
--
-- 'name', 'acl_name' - The name of the Access Control List
--
-- 'pendingChanges', 'acl_pendingChanges' - A list of updates being applied to the ACL.
--
-- 'status', 'acl_status' - Indicates ACL status. Can be \"creating\", \"active\", \"modifying\",
-- \"deleting\".
--
-- 'userNames', 'acl_userNames' - The list of user names that belong to the ACL.
newACL ::
  ACL
newACL =
  ACL'
    { arn = Prelude.Nothing,
      clusters = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      pendingChanges = Prelude.Nothing,
      status = Prelude.Nothing,
      userNames = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the ACL
acl_arn :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_arn = Lens.lens (\ACL' {arn} -> arn) (\s@ACL' {} a -> s {arn = a} :: ACL)

-- | A list of clusters associated with the ACL.
acl_clusters :: Lens.Lens' ACL (Prelude.Maybe [Prelude.Text])
acl_clusters = Lens.lens (\ACL' {clusters} -> clusters) (\s@ACL' {} a -> s {clusters = a} :: ACL) Prelude.. Lens.mapping Lens.coerced

-- | The minimum engine version supported for the ACL
acl_minimumEngineVersion :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_minimumEngineVersion = Lens.lens (\ACL' {minimumEngineVersion} -> minimumEngineVersion) (\s@ACL' {} a -> s {minimumEngineVersion = a} :: ACL)

-- | The name of the Access Control List
acl_name :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_name = Lens.lens (\ACL' {name} -> name) (\s@ACL' {} a -> s {name = a} :: ACL)

-- | A list of updates being applied to the ACL.
acl_pendingChanges :: Lens.Lens' ACL (Prelude.Maybe ACLPendingChanges)
acl_pendingChanges = Lens.lens (\ACL' {pendingChanges} -> pendingChanges) (\s@ACL' {} a -> s {pendingChanges = a} :: ACL)

-- | Indicates ACL status. Can be \"creating\", \"active\", \"modifying\",
-- \"deleting\".
acl_status :: Lens.Lens' ACL (Prelude.Maybe Prelude.Text)
acl_status = Lens.lens (\ACL' {status} -> status) (\s@ACL' {} a -> s {status = a} :: ACL)

-- | The list of user names that belong to the ACL.
acl_userNames :: Lens.Lens' ACL (Prelude.Maybe [Prelude.Text])
acl_userNames = Lens.lens (\ACL' {userNames} -> userNames) (\s@ACL' {} a -> s {userNames = a} :: ACL) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ACL where
  parseJSON =
    Data.withObject
      "ACL"
      ( \x ->
          ACL'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Clusters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MinimumEngineVersion")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PendingChanges")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UserNames" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ACL where
  hashWithSalt _salt ACL' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` clusters
      `Prelude.hashWithSalt` minimumEngineVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pendingChanges
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userNames

instance Prelude.NFData ACL where
  rnf ACL' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf clusters
      `Prelude.seq` Prelude.rnf minimumEngineVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pendingChanges
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf userNames
