{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Neptune.ModifyGlobalCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Neptune global cluster. You can change
-- one or more database configuration parameters by specifying these
-- parameters and their new values in the request.
module Amazonka.Neptune.ModifyGlobalCluster
  ( -- * Creating a Request
    ModifyGlobalCluster (..),
    newModifyGlobalCluster,

    -- * Request Lenses
    modifyGlobalCluster_allowMajorVersionUpgrade,
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_engineVersion,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_globalClusterIdentifier,

    -- * Destructuring the Response
    ModifyGlobalClusterResponse (..),
    newModifyGlobalClusterResponse,

    -- * Response Lenses
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { -- | A value that indicates whether major version upgrades are allowed.
    --
    -- Constraints: You must allow major version upgrades if you specify a
    -- value for the @EngineVersion@ parameter that is a different major
    -- version than the DB cluster\'s current version.
    --
    -- If you upgrade the major version of a global database, the cluster and
    -- DB instance parameter groups are set to the default parameter groups for
    -- the new version, so you will need to apply any custom parameter groups
    -- after completing the upgrade.
    allowMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the global database has deletion protection enabled.
    -- The global database cannot be deleted when deletion protection is
    -- enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to which you want to upgrade.
    -- Changing this parameter will result in an outage. The change is applied
    -- during the next maintenance window unless @ApplyImmediately@ is enabled.
    --
    -- To list all of the available Neptune engine versions, use the following
    -- command:
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A new cluster identifier to assign to the global database. This value is
    -- stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-cluster2@
    newGlobalClusterIdentifier' :: Prelude.Maybe Prelude.Text,
    -- | The DB cluster identifier for the global cluster being modified. This
    -- parameter is not case-sensitive.
    --
    -- Constraints: Must match the identifier of an existing global database
    -- cluster.
    globalClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowMajorVersionUpgrade', 'modifyGlobalCluster_allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades if you specify a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- If you upgrade the major version of a global database, the cluster and
-- DB instance parameter groups are set to the default parameter groups for
-- the new version, so you will need to apply any custom parameter groups
-- after completing the upgrade.
--
-- 'deletionProtection', 'modifyGlobalCluster_deletionProtection' - Indicates whether the global database has deletion protection enabled.
-- The global database cannot be deleted when deletion protection is
-- enabled.
--
-- 'engineVersion', 'modifyGlobalCluster_engineVersion' - The version number of the database engine to which you want to upgrade.
-- Changing this parameter will result in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available Neptune engine versions, use the following
-- command:
--
-- 'newGlobalClusterIdentifier'', 'modifyGlobalCluster_newGlobalClusterIdentifier' - A new cluster identifier to assign to the global database. This value is
-- stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
--
-- 'globalClusterIdentifier', 'modifyGlobalCluster_globalClusterIdentifier' - The DB cluster identifier for the global cluster being modified. This
-- parameter is not case-sensitive.
--
-- Constraints: Must match the identifier of an existing global database
-- cluster.
newModifyGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Prelude.Text ->
  ModifyGlobalCluster
newModifyGlobalCluster pGlobalClusterIdentifier_ =
  ModifyGlobalCluster'
    { allowMajorVersionUpgrade =
        Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      newGlobalClusterIdentifier' = Prelude.Nothing,
      globalClusterIdentifier = pGlobalClusterIdentifier_
    }

-- | A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades if you specify a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- If you upgrade the major version of a global database, the cluster and
-- DB instance parameter groups are set to the default parameter groups for
-- the new version, so you will need to apply any custom parameter groups
-- after completing the upgrade.
modifyGlobalCluster_allowMajorVersionUpgrade :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Bool)
modifyGlobalCluster_allowMajorVersionUpgrade = Lens.lens (\ModifyGlobalCluster' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyGlobalCluster' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyGlobalCluster)

-- | Indicates whether the global database has deletion protection enabled.
-- The global database cannot be deleted when deletion protection is
-- enabled.
modifyGlobalCluster_deletionProtection :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Bool)
modifyGlobalCluster_deletionProtection = Lens.lens (\ModifyGlobalCluster' {deletionProtection} -> deletionProtection) (\s@ModifyGlobalCluster' {} a -> s {deletionProtection = a} :: ModifyGlobalCluster)

-- | The version number of the database engine to which you want to upgrade.
-- Changing this parameter will result in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available Neptune engine versions, use the following
-- command:
modifyGlobalCluster_engineVersion :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Text)
modifyGlobalCluster_engineVersion = Lens.lens (\ModifyGlobalCluster' {engineVersion} -> engineVersion) (\s@ModifyGlobalCluster' {} a -> s {engineVersion = a} :: ModifyGlobalCluster)

-- | A new cluster identifier to assign to the global database. This value is
-- stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
modifyGlobalCluster_newGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Text)
modifyGlobalCluster_newGlobalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {newGlobalClusterIdentifier'} -> newGlobalClusterIdentifier') (\s@ModifyGlobalCluster' {} a -> s {newGlobalClusterIdentifier' = a} :: ModifyGlobalCluster)

-- | The DB cluster identifier for the global cluster being modified. This
-- parameter is not case-sensitive.
--
-- Constraints: Must match the identifier of an existing global database
-- cluster.
modifyGlobalCluster_globalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster Prelude.Text
modifyGlobalCluster_globalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@ModifyGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: ModifyGlobalCluster)

instance Core.AWSRequest ModifyGlobalCluster where
  type
    AWSResponse ModifyGlobalCluster =
      ModifyGlobalClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyGlobalClusterResult"
      ( \s h x ->
          ModifyGlobalClusterResponse'
            Prelude.<$> (x Data..@? "GlobalCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyGlobalCluster where
  hashWithSalt _salt ModifyGlobalCluster' {..} =
    _salt
      `Prelude.hashWithSalt` allowMajorVersionUpgrade
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` newGlobalClusterIdentifier'
      `Prelude.hashWithSalt` globalClusterIdentifier

instance Prelude.NFData ModifyGlobalCluster where
  rnf ModifyGlobalCluster' {..} =
    Prelude.rnf allowMajorVersionUpgrade
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf newGlobalClusterIdentifier'
      `Prelude.seq` Prelude.rnf globalClusterIdentifier

instance Data.ToHeaders ModifyGlobalCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyGlobalCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyGlobalCluster where
  toQuery ModifyGlobalCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyGlobalCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllowMajorVersionUpgrade"
          Data.=: allowMajorVersionUpgrade,
        "DeletionProtection" Data.=: deletionProtection,
        "EngineVersion" Data.=: engineVersion,
        "NewGlobalClusterIdentifier"
          Data.=: newGlobalClusterIdentifier',
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier
      ]

-- | /See:/ 'newModifyGlobalClusterResponse' smart constructor.
data ModifyGlobalClusterResponse = ModifyGlobalClusterResponse'
  { globalCluster :: Prelude.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyGlobalClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalCluster', 'modifyGlobalClusterResponse_globalCluster' - Undocumented member.
--
-- 'httpStatus', 'modifyGlobalClusterResponse_httpStatus' - The response's http status code.
newModifyGlobalClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyGlobalClusterResponse
newModifyGlobalClusterResponse pHttpStatus_ =
  ModifyGlobalClusterResponse'
    { globalCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyGlobalClusterResponse_globalCluster :: Lens.Lens' ModifyGlobalClusterResponse (Prelude.Maybe GlobalCluster)
modifyGlobalClusterResponse_globalCluster = Lens.lens (\ModifyGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@ModifyGlobalClusterResponse' {} a -> s {globalCluster = a} :: ModifyGlobalClusterResponse)

-- | The response's http status code.
modifyGlobalClusterResponse_httpStatus :: Lens.Lens' ModifyGlobalClusterResponse Prelude.Int
modifyGlobalClusterResponse_httpStatus = Lens.lens (\ModifyGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyGlobalClusterResponse' {} a -> s {httpStatus = a} :: ModifyGlobalClusterResponse)

instance Prelude.NFData ModifyGlobalClusterResponse where
  rnf ModifyGlobalClusterResponse' {..} =
    Prelude.rnf globalCluster
      `Prelude.seq` Prelude.rnf httpStatus
