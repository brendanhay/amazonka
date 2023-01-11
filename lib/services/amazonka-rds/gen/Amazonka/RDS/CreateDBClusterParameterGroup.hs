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
-- Module      : Amazonka.RDS.CreateDBClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster parameter group.
--
-- Parameters in a DB cluster parameter group apply to all of the instances
-- in a DB cluster.
--
-- A DB cluster parameter group is initially created with the default
-- parameters for the database engine used by instances in the DB cluster.
-- To provide custom values for any of the parameters, you must modify the
-- group after creating it using @ModifyDBClusterParameterGroup@. Once
-- you\'ve created a DB cluster parameter group, you need to associate it
-- with your DB cluster using @ModifyDBCluster@.
--
-- When you associate a new DB cluster parameter group with a running
-- Aurora DB cluster, reboot the DB instances in the DB cluster without
-- failover for the new DB cluster parameter group and associated settings
-- to take effect.
--
-- When you associate a new DB cluster parameter group with a running
-- Multi-AZ DB cluster, reboot the DB cluster without failover for the new
-- DB cluster parameter group and associated settings to take effect.
--
-- After you create a DB cluster parameter group, you should wait at least
-- 5 minutes before creating your first DB cluster that uses that DB
-- cluster parameter group as the default parameter group. This allows
-- Amazon RDS to fully complete the create action before the DB cluster
-- parameter group is used as the default for a new DB cluster. This is
-- especially important for parameters that are critical when creating the
-- default database for a DB cluster, such as the character set for the
-- default database defined by the @character_set_database@ parameter. You
-- can use the /Parameter Groups/ option of the
-- <https://console.aws.amazon.com/rds/ Amazon RDS console> or the
-- @DescribeDBClusterParameters@ operation to verify that your DB cluster
-- parameter group has been created or modified.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.CreateDBClusterParameterGroup
  ( -- * Creating a Request
    CreateDBClusterParameterGroup (..),
    newCreateDBClusterParameterGroup,

    -- * Request Lenses
    createDBClusterParameterGroup_tags,
    createDBClusterParameterGroup_dbClusterParameterGroupName,
    createDBClusterParameterGroup_dbParameterGroupFamily,
    createDBClusterParameterGroup_description,

    -- * Destructuring the Response
    CreateDBClusterParameterGroupResponse (..),
    newCreateDBClusterParameterGroupResponse,

    -- * Response Lenses
    createDBClusterParameterGroupResponse_dbClusterParameterGroup,
    createDBClusterParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateDBClusterParameterGroup' smart constructor.
data CreateDBClusterParameterGroup = CreateDBClusterParameterGroup'
  { -- | Tags to assign to the DB cluster parameter group.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the DB cluster parameter group.
    --
    -- Constraints:
    --
    -- -   Must not match the name of an existing DB cluster parameter group.
    --
    -- This value is stored as a lowercase string.
    dbClusterParameterGroupName :: Prelude.Text,
    -- | The DB cluster parameter group family name. A DB cluster parameter group
    -- can be associated with one and only one DB cluster parameter group
    -- family, and can be applied only to a DB cluster running a database
    -- engine and engine version compatible with that DB cluster parameter
    -- group family.
    --
    -- __Aurora MySQL__
    --
    -- Example: @aurora5.6@, @aurora-mysql5.7@, @aurora-mysql8.0@
    --
    -- __Aurora PostgreSQL__
    --
    -- Example: @aurora-postgresql9.6@
    --
    -- __RDS for MySQL__
    --
    -- Example: @mysql8.0@
    --
    -- __RDS for PostgreSQL__
    --
    -- Example: @postgres12@
    --
    -- To list all of the available parameter group families for a DB engine,
    -- use the following command:
    --
    -- @aws rds describe-db-engine-versions --query \"DBEngineVersions[].DBParameterGroupFamily\" --engine \<engine>@
    --
    -- For example, to list all of the available parameter group families for
    -- the Aurora PostgreSQL DB engine, use the following command:
    --
    -- @aws rds describe-db-engine-versions --query \"DBEngineVersions[].DBParameterGroupFamily\" --engine aurora-postgresql@
    --
    -- The output contains duplicates.
    --
    -- The following are the valid DB engine values:
    --
    -- -   @aurora@ (for MySQL 5.6-compatible Aurora)
    --
    -- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
    --     Aurora)
    --
    -- -   @aurora-postgresql@
    --
    -- -   @mysql@
    --
    -- -   @postgres@
    dbParameterGroupFamily :: Prelude.Text,
    -- | The description for the DB cluster parameter group.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDBClusterParameterGroup_tags' - Tags to assign to the DB cluster parameter group.
--
-- 'dbClusterParameterGroupName', 'createDBClusterParameterGroup_dbClusterParameterGroupName' - The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must not match the name of an existing DB cluster parameter group.
--
-- This value is stored as a lowercase string.
--
-- 'dbParameterGroupFamily', 'createDBClusterParameterGroup_dbParameterGroupFamily' - The DB cluster parameter group family name. A DB cluster parameter group
-- can be associated with one and only one DB cluster parameter group
-- family, and can be applied only to a DB cluster running a database
-- engine and engine version compatible with that DB cluster parameter
-- group family.
--
-- __Aurora MySQL__
--
-- Example: @aurora5.6@, @aurora-mysql5.7@, @aurora-mysql8.0@
--
-- __Aurora PostgreSQL__
--
-- Example: @aurora-postgresql9.6@
--
-- __RDS for MySQL__
--
-- Example: @mysql8.0@
--
-- __RDS for PostgreSQL__
--
-- Example: @postgres12@
--
-- To list all of the available parameter group families for a DB engine,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --query \"DBEngineVersions[].DBParameterGroupFamily\" --engine \<engine>@
--
-- For example, to list all of the available parameter group families for
-- the Aurora PostgreSQL DB engine, use the following command:
--
-- @aws rds describe-db-engine-versions --query \"DBEngineVersions[].DBParameterGroupFamily\" --engine aurora-postgresql@
--
-- The output contains duplicates.
--
-- The following are the valid DB engine values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
--     Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mysql@
--
-- -   @postgres@
--
-- 'description', 'createDBClusterParameterGroup_description' - The description for the DB cluster parameter group.
newCreateDBClusterParameterGroup ::
  -- | 'dbClusterParameterGroupName'
  Prelude.Text ->
  -- | 'dbParameterGroupFamily'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateDBClusterParameterGroup
newCreateDBClusterParameterGroup
  pDBClusterParameterGroupName_
  pDBParameterGroupFamily_
  pDescription_ =
    CreateDBClusterParameterGroup'
      { tags =
          Prelude.Nothing,
        dbClusterParameterGroupName =
          pDBClusterParameterGroupName_,
        dbParameterGroupFamily =
          pDBParameterGroupFamily_,
        description = pDescription_
      }

-- | Tags to assign to the DB cluster parameter group.
createDBClusterParameterGroup_tags :: Lens.Lens' CreateDBClusterParameterGroup (Prelude.Maybe [Tag])
createDBClusterParameterGroup_tags = Lens.lens (\CreateDBClusterParameterGroup' {tags} -> tags) (\s@CreateDBClusterParameterGroup' {} a -> s {tags = a} :: CreateDBClusterParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must not match the name of an existing DB cluster parameter group.
--
-- This value is stored as a lowercase string.
createDBClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' CreateDBClusterParameterGroup Prelude.Text
createDBClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\CreateDBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@CreateDBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: CreateDBClusterParameterGroup)

-- | The DB cluster parameter group family name. A DB cluster parameter group
-- can be associated with one and only one DB cluster parameter group
-- family, and can be applied only to a DB cluster running a database
-- engine and engine version compatible with that DB cluster parameter
-- group family.
--
-- __Aurora MySQL__
--
-- Example: @aurora5.6@, @aurora-mysql5.7@, @aurora-mysql8.0@
--
-- __Aurora PostgreSQL__
--
-- Example: @aurora-postgresql9.6@
--
-- __RDS for MySQL__
--
-- Example: @mysql8.0@
--
-- __RDS for PostgreSQL__
--
-- Example: @postgres12@
--
-- To list all of the available parameter group families for a DB engine,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --query \"DBEngineVersions[].DBParameterGroupFamily\" --engine \<engine>@
--
-- For example, to list all of the available parameter group families for
-- the Aurora PostgreSQL DB engine, use the following command:
--
-- @aws rds describe-db-engine-versions --query \"DBEngineVersions[].DBParameterGroupFamily\" --engine aurora-postgresql@
--
-- The output contains duplicates.
--
-- The following are the valid DB engine values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
--     Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mysql@
--
-- -   @postgres@
createDBClusterParameterGroup_dbParameterGroupFamily :: Lens.Lens' CreateDBClusterParameterGroup Prelude.Text
createDBClusterParameterGroup_dbParameterGroupFamily = Lens.lens (\CreateDBClusterParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@CreateDBClusterParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: CreateDBClusterParameterGroup)

-- | The description for the DB cluster parameter group.
createDBClusterParameterGroup_description :: Lens.Lens' CreateDBClusterParameterGroup Prelude.Text
createDBClusterParameterGroup_description = Lens.lens (\CreateDBClusterParameterGroup' {description} -> description) (\s@CreateDBClusterParameterGroup' {} a -> s {description = a} :: CreateDBClusterParameterGroup)

instance
  Core.AWSRequest
    CreateDBClusterParameterGroup
  where
  type
    AWSResponse CreateDBClusterParameterGroup =
      CreateDBClusterParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterParameterGroupResult"
      ( \s h x ->
          CreateDBClusterParameterGroupResponse'
            Prelude.<$> (x Data..@? "DBClusterParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDBClusterParameterGroup
  where
  hashWithSalt _salt CreateDBClusterParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateDBClusterParameterGroup where
  rnf CreateDBClusterParameterGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders CreateDBClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBClusterParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBClusterParameterGroup where
  toQuery CreateDBClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateDBClusterParameterGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBParameterGroupFamily"
          Data.=: dbParameterGroupFamily,
        "Description" Data.=: description
      ]

-- | /See:/ 'newCreateDBClusterParameterGroupResponse' smart constructor.
data CreateDBClusterParameterGroupResponse = CreateDBClusterParameterGroupResponse'
  { dbClusterParameterGroup :: Prelude.Maybe DBClusterParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroup', 'createDBClusterParameterGroupResponse_dbClusterParameterGroup' - Undocumented member.
--
-- 'httpStatus', 'createDBClusterParameterGroupResponse_httpStatus' - The response's http status code.
newCreateDBClusterParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBClusterParameterGroupResponse
newCreateDBClusterParameterGroupResponse pHttpStatus_ =
  CreateDBClusterParameterGroupResponse'
    { dbClusterParameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBClusterParameterGroupResponse_dbClusterParameterGroup :: Lens.Lens' CreateDBClusterParameterGroupResponse (Prelude.Maybe DBClusterParameterGroup)
createDBClusterParameterGroupResponse_dbClusterParameterGroup = Lens.lens (\CreateDBClusterParameterGroupResponse' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@CreateDBClusterParameterGroupResponse' {} a -> s {dbClusterParameterGroup = a} :: CreateDBClusterParameterGroupResponse)

-- | The response's http status code.
createDBClusterParameterGroupResponse_httpStatus :: Lens.Lens' CreateDBClusterParameterGroupResponse Prelude.Int
createDBClusterParameterGroupResponse_httpStatus = Lens.lens (\CreateDBClusterParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDBClusterParameterGroupResponse' {} a -> s {httpStatus = a} :: CreateDBClusterParameterGroupResponse)

instance
  Prelude.NFData
    CreateDBClusterParameterGroupResponse
  where
  rnf CreateDBClusterParameterGroupResponse' {..} =
    Prelude.rnf dbClusterParameterGroup
      `Prelude.seq` Prelude.rnf httpStatus
