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
-- Module      : Amazonka.RDS.ModifyGlobalCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora global cluster. You can change one
-- or more database configuration parameters by specifying these parameters
-- and the new values in the request. For more information on Amazon
-- Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- This action only applies to Aurora DB clusters.
module Amazonka.RDS.ModifyGlobalCluster
  ( -- * Creating a Request
    ModifyGlobalCluster (..),
    newModifyGlobalCluster,

    -- * Request Lenses
    modifyGlobalCluster_allowMajorVersionUpgrade,
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_engineVersion,
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalCluster_newGlobalClusterIdentifier,

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
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { -- | A value that indicates whether major version upgrades are allowed.
    --
    -- Constraints: You must allow major version upgrades when specifying a
    -- value for the @EngineVersion@ parameter that is a different major
    -- version than the DB cluster\'s current version.
    --
    -- If you upgrade the major version of a global database, the cluster and
    -- DB instance parameter groups are set to the default parameter groups for
    -- the new version. Apply any custom parameter groups after completing the
    -- upgrade.
    allowMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if the global database cluster has deletion protection
    -- enabled. The global database cluster can\'t be deleted when deletion
    -- protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to which you want to upgrade.
    -- Changing this parameter results in an outage. The change is applied
    -- during the next maintenance window unless @ApplyImmediately@ is enabled.
    --
    -- To list all of the available engine versions for @aurora-mysql@ (for
    -- MySQL-based Aurora global databases), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
    --
    -- To list all of the available engine versions for @aurora-postgresql@
    -- (for PostgreSQL-based Aurora global databases), use the following
    -- command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The DB cluster identifier for the global cluster being modified. This
    -- parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing global database cluster.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The new cluster identifier for the global database cluster when
    -- modifying a global database cluster. This value is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   The first character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-cluster2@
    newGlobalClusterIdentifier' :: Prelude.Maybe Prelude.Text
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
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- If you upgrade the major version of a global database, the cluster and
-- DB instance parameter groups are set to the default parameter groups for
-- the new version. Apply any custom parameter groups after completing the
-- upgrade.
--
-- 'deletionProtection', 'modifyGlobalCluster_deletionProtection' - Indicates if the global database cluster has deletion protection
-- enabled. The global database cluster can\'t be deleted when deletion
-- protection is enabled.
--
-- 'engineVersion', 'modifyGlobalCluster_engineVersion' - The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL-based Aurora global databases), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
--
-- To list all of the available engine versions for @aurora-postgresql@
-- (for PostgreSQL-based Aurora global databases), use the following
-- command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
--
-- 'globalClusterIdentifier', 'modifyGlobalCluster_globalClusterIdentifier' - The DB cluster identifier for the global cluster being modified. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing global database cluster.
--
-- 'newGlobalClusterIdentifier'', 'modifyGlobalCluster_newGlobalClusterIdentifier' - The new cluster identifier for the global database cluster when
-- modifying a global database cluster. This value is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   The first character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
newModifyGlobalCluster ::
  ModifyGlobalCluster
newModifyGlobalCluster =
  ModifyGlobalCluster'
    { allowMajorVersionUpgrade =
        Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      newGlobalClusterIdentifier' = Prelude.Nothing
    }

-- | A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- If you upgrade the major version of a global database, the cluster and
-- DB instance parameter groups are set to the default parameter groups for
-- the new version. Apply any custom parameter groups after completing the
-- upgrade.
modifyGlobalCluster_allowMajorVersionUpgrade :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Bool)
modifyGlobalCluster_allowMajorVersionUpgrade = Lens.lens (\ModifyGlobalCluster' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyGlobalCluster' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyGlobalCluster)

-- | Indicates if the global database cluster has deletion protection
-- enabled. The global database cluster can\'t be deleted when deletion
-- protection is enabled.
modifyGlobalCluster_deletionProtection :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Bool)
modifyGlobalCluster_deletionProtection = Lens.lens (\ModifyGlobalCluster' {deletionProtection} -> deletionProtection) (\s@ModifyGlobalCluster' {} a -> s {deletionProtection = a} :: ModifyGlobalCluster)

-- | The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL-based Aurora global databases), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
--
-- To list all of the available engine versions for @aurora-postgresql@
-- (for PostgreSQL-based Aurora global databases), use the following
-- command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
modifyGlobalCluster_engineVersion :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Text)
modifyGlobalCluster_engineVersion = Lens.lens (\ModifyGlobalCluster' {engineVersion} -> engineVersion) (\s@ModifyGlobalCluster' {} a -> s {engineVersion = a} :: ModifyGlobalCluster)

-- | The DB cluster identifier for the global cluster being modified. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing global database cluster.
modifyGlobalCluster_globalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Text)
modifyGlobalCluster_globalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@ModifyGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: ModifyGlobalCluster)

-- | The new cluster identifier for the global database cluster when
-- modifying a global database cluster. This value is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   The first character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
modifyGlobalCluster_newGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Prelude.Maybe Prelude.Text)
modifyGlobalCluster_newGlobalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {newGlobalClusterIdentifier'} -> newGlobalClusterIdentifier') (\s@ModifyGlobalCluster' {} a -> s {newGlobalClusterIdentifier' = a} :: ModifyGlobalCluster)

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
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` newGlobalClusterIdentifier'

instance Prelude.NFData ModifyGlobalCluster where
  rnf ModifyGlobalCluster' {..} =
    Prelude.rnf allowMajorVersionUpgrade
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf globalClusterIdentifier
      `Prelude.seq` Prelude.rnf newGlobalClusterIdentifier'

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
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier,
        "NewGlobalClusterIdentifier"
          Data.=: newGlobalClusterIdentifier'
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
