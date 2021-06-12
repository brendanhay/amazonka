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
-- Module      : Network.AWS.RDS.ModifyGlobalCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora global cluster. You can change one
-- or more database configuration parameters by specifying these parameters
-- and the new values in the request. For more information on Amazon
-- Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.ModifyGlobalCluster
  ( -- * Creating a Request
    ModifyGlobalCluster (..),
    newModifyGlobalCluster,

    -- * Request Lenses
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_allowMajorVersionUpgrade,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { -- | Indicates if the global database cluster has deletion protection
    -- enabled. The global database cluster can\'t be deleted when deletion
    -- protection is enabled.
    deletionProtection :: Core.Maybe Core.Bool,
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
    allowMajorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The version number of the database engine to which you want to upgrade.
    -- Changing this parameter results in an outage. The change is applied
    -- during the next maintenance window unless @ApplyImmediately@ is enabled.
    --
    -- To list all of the available engine versions for @aurora@ (for MySQL
    -- 5.6-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
    --
    -- To list all of the available engine versions for @aurora-mysql@ (for
    -- MySQL 5.7-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
    --
    -- To list all of the available engine versions for @aurora-postgresql@,
    -- use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
    engineVersion :: Core.Maybe Core.Text,
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
    newGlobalClusterIdentifier' :: Core.Maybe Core.Text,
    -- | The DB cluster identifier for the global cluster being modified. This
    -- parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing global database cluster.
    globalClusterIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'modifyGlobalCluster_deletionProtection' - Indicates if the global database cluster has deletion protection
-- enabled. The global database cluster can\'t be deleted when deletion
-- protection is enabled.
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
-- 'engineVersion', 'modifyGlobalCluster_engineVersion' - The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
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
--
-- 'globalClusterIdentifier', 'modifyGlobalCluster_globalClusterIdentifier' - The DB cluster identifier for the global cluster being modified. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing global database cluster.
newModifyGlobalCluster ::
  ModifyGlobalCluster
newModifyGlobalCluster =
  ModifyGlobalCluster'
    { deletionProtection =
        Core.Nothing,
      allowMajorVersionUpgrade = Core.Nothing,
      engineVersion = Core.Nothing,
      newGlobalClusterIdentifier' = Core.Nothing,
      globalClusterIdentifier = Core.Nothing
    }

-- | Indicates if the global database cluster has deletion protection
-- enabled. The global database cluster can\'t be deleted when deletion
-- protection is enabled.
modifyGlobalCluster_deletionProtection :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Bool)
modifyGlobalCluster_deletionProtection = Lens.lens (\ModifyGlobalCluster' {deletionProtection} -> deletionProtection) (\s@ModifyGlobalCluster' {} a -> s {deletionProtection = a} :: ModifyGlobalCluster)

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
modifyGlobalCluster_allowMajorVersionUpgrade :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Bool)
modifyGlobalCluster_allowMajorVersionUpgrade = Lens.lens (\ModifyGlobalCluster' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyGlobalCluster' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyGlobalCluster)

-- | The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \'*[]|[?SupportsGlobalDatabases == \`true\`].[EngineVersion]\'@
modifyGlobalCluster_engineVersion :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Text)
modifyGlobalCluster_engineVersion = Lens.lens (\ModifyGlobalCluster' {engineVersion} -> engineVersion) (\s@ModifyGlobalCluster' {} a -> s {engineVersion = a} :: ModifyGlobalCluster)

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
modifyGlobalCluster_newGlobalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Text)
modifyGlobalCluster_newGlobalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {newGlobalClusterIdentifier'} -> newGlobalClusterIdentifier') (\s@ModifyGlobalCluster' {} a -> s {newGlobalClusterIdentifier' = a} :: ModifyGlobalCluster)

-- | The DB cluster identifier for the global cluster being modified. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing global database cluster.
modifyGlobalCluster_globalClusterIdentifier :: Lens.Lens' ModifyGlobalCluster (Core.Maybe Core.Text)
modifyGlobalCluster_globalClusterIdentifier = Lens.lens (\ModifyGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@ModifyGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: ModifyGlobalCluster)

instance Core.AWSRequest ModifyGlobalCluster where
  type
    AWSResponse ModifyGlobalCluster =
      ModifyGlobalClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyGlobalClusterResult"
      ( \s h x ->
          ModifyGlobalClusterResponse'
            Core.<$> (x Core..@? "GlobalCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyGlobalCluster

instance Core.NFData ModifyGlobalCluster

instance Core.ToHeaders ModifyGlobalCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyGlobalCluster where
  toPath = Core.const "/"

instance Core.ToQuery ModifyGlobalCluster where
  toQuery ModifyGlobalCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyGlobalCluster" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "AllowMajorVersionUpgrade"
          Core.=: allowMajorVersionUpgrade,
        "EngineVersion" Core.=: engineVersion,
        "NewGlobalClusterIdentifier"
          Core.=: newGlobalClusterIdentifier',
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier
      ]

-- | /See:/ 'newModifyGlobalClusterResponse' smart constructor.
data ModifyGlobalClusterResponse = ModifyGlobalClusterResponse'
  { globalCluster :: Core.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyGlobalClusterResponse
newModifyGlobalClusterResponse pHttpStatus_ =
  ModifyGlobalClusterResponse'
    { globalCluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyGlobalClusterResponse_globalCluster :: Lens.Lens' ModifyGlobalClusterResponse (Core.Maybe GlobalCluster)
modifyGlobalClusterResponse_globalCluster = Lens.lens (\ModifyGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@ModifyGlobalClusterResponse' {} a -> s {globalCluster = a} :: ModifyGlobalClusterResponse)

-- | The response's http status code.
modifyGlobalClusterResponse_httpStatus :: Lens.Lens' ModifyGlobalClusterResponse Core.Int
modifyGlobalClusterResponse_httpStatus = Lens.lens (\ModifyGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyGlobalClusterResponse' {} a -> s {httpStatus = a} :: ModifyGlobalClusterResponse)

instance Core.NFData ModifyGlobalClusterResponse
