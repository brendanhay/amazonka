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
-- Module      : Amazonka.Neptune.CreateDBParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB parameter group.
--
-- A DB parameter group is initially created with the default parameters
-- for the database engine used by the DB instance. To provide custom
-- values for any of the parameters, you must modify the group after
-- creating it using /ModifyDBParameterGroup/. Once you\'ve created a DB
-- parameter group, you need to associate it with your DB instance using
-- /ModifyDBInstance/. When you associate a new DB parameter group with a
-- running DB instance, you need to reboot the DB instance without failover
-- for the new DB parameter group and associated settings to take effect.
--
-- After you create a DB parameter group, you should wait at least 5
-- minutes before creating your first DB instance that uses that DB
-- parameter group as the default parameter group. This allows Amazon
-- Neptune to fully complete the create action before the parameter group
-- is used as the default for a new DB instance. This is especially
-- important for parameters that are critical when creating the default
-- database for a DB instance, such as the character set for the default
-- database defined by the @character_set_database@ parameter. You can use
-- the /Parameter Groups/ option of the Amazon Neptune console or the
-- /DescribeDBParameters/ command to verify that your DB parameter group
-- has been created or modified.
module Amazonka.Neptune.CreateDBParameterGroup
  ( -- * Creating a Request
    CreateDBParameterGroup (..),
    newCreateDBParameterGroup,

    -- * Request Lenses
    createDBParameterGroup_tags,
    createDBParameterGroup_dbParameterGroupName,
    createDBParameterGroup_dbParameterGroupFamily,
    createDBParameterGroup_description,

    -- * Destructuring the Response
    CreateDBParameterGroupResponse (..),
    newCreateDBParameterGroupResponse,

    -- * Response Lenses
    createDBParameterGroupResponse_dbParameterGroup,
    createDBParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBParameterGroup' smart constructor.
data CreateDBParameterGroup = CreateDBParameterGroup'
  { -- | The tags to be assigned to the new DB parameter group.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    --
    -- This value is stored as a lowercase string.
    dbParameterGroupName :: Prelude.Text,
    -- | The DB parameter group family name. A DB parameter group can be
    -- associated with one and only one DB parameter group family, and can be
    -- applied only to a DB instance running a database engine and engine
    -- version compatible with that DB parameter group family.
    dbParameterGroupFamily :: Prelude.Text,
    -- | The description for the DB parameter group.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDBParameterGroup_tags' - The tags to be assigned to the new DB parameter group.
--
-- 'dbParameterGroupName', 'createDBParameterGroup_dbParameterGroupName' - The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- This value is stored as a lowercase string.
--
-- 'dbParameterGroupFamily', 'createDBParameterGroup_dbParameterGroupFamily' - The DB parameter group family name. A DB parameter group can be
-- associated with one and only one DB parameter group family, and can be
-- applied only to a DB instance running a database engine and engine
-- version compatible with that DB parameter group family.
--
-- 'description', 'createDBParameterGroup_description' - The description for the DB parameter group.
newCreateDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Prelude.Text ->
  -- | 'dbParameterGroupFamily'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateDBParameterGroup
newCreateDBParameterGroup
  pDBParameterGroupName_
  pDBParameterGroupFamily_
  pDescription_ =
    CreateDBParameterGroup'
      { tags = Prelude.Nothing,
        dbParameterGroupName = pDBParameterGroupName_,
        dbParameterGroupFamily = pDBParameterGroupFamily_,
        description = pDescription_
      }

-- | The tags to be assigned to the new DB parameter group.
createDBParameterGroup_tags :: Lens.Lens' CreateDBParameterGroup (Prelude.Maybe [Tag])
createDBParameterGroup_tags = Lens.lens (\CreateDBParameterGroup' {tags} -> tags) (\s@CreateDBParameterGroup' {} a -> s {tags = a} :: CreateDBParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- This value is stored as a lowercase string.
createDBParameterGroup_dbParameterGroupName :: Lens.Lens' CreateDBParameterGroup Prelude.Text
createDBParameterGroup_dbParameterGroupName = Lens.lens (\CreateDBParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@CreateDBParameterGroup' {} a -> s {dbParameterGroupName = a} :: CreateDBParameterGroup)

-- | The DB parameter group family name. A DB parameter group can be
-- associated with one and only one DB parameter group family, and can be
-- applied only to a DB instance running a database engine and engine
-- version compatible with that DB parameter group family.
createDBParameterGroup_dbParameterGroupFamily :: Lens.Lens' CreateDBParameterGroup Prelude.Text
createDBParameterGroup_dbParameterGroupFamily = Lens.lens (\CreateDBParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@CreateDBParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: CreateDBParameterGroup)

-- | The description for the DB parameter group.
createDBParameterGroup_description :: Lens.Lens' CreateDBParameterGroup Prelude.Text
createDBParameterGroup_description = Lens.lens (\CreateDBParameterGroup' {description} -> description) (\s@CreateDBParameterGroup' {} a -> s {description = a} :: CreateDBParameterGroup)

instance Core.AWSRequest CreateDBParameterGroup where
  type
    AWSResponse CreateDBParameterGroup =
      CreateDBParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBParameterGroupResult"
      ( \s h x ->
          CreateDBParameterGroupResponse'
            Prelude.<$> (x Data..@? "DBParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBParameterGroup where
  hashWithSalt _salt CreateDBParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateDBParameterGroup where
  rnf CreateDBParameterGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders CreateDBParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBParameterGroup where
  toQuery CreateDBParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "DBParameterGroupFamily"
          Data.=: dbParameterGroupFamily,
        "Description" Data.=: description
      ]

-- | /See:/ 'newCreateDBParameterGroupResponse' smart constructor.
data CreateDBParameterGroupResponse = CreateDBParameterGroupResponse'
  { dbParameterGroup :: Prelude.Maybe DBParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroup', 'createDBParameterGroupResponse_dbParameterGroup' - Undocumented member.
--
-- 'httpStatus', 'createDBParameterGroupResponse_httpStatus' - The response's http status code.
newCreateDBParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBParameterGroupResponse
newCreateDBParameterGroupResponse pHttpStatus_ =
  CreateDBParameterGroupResponse'
    { dbParameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBParameterGroupResponse_dbParameterGroup :: Lens.Lens' CreateDBParameterGroupResponse (Prelude.Maybe DBParameterGroup)
createDBParameterGroupResponse_dbParameterGroup = Lens.lens (\CreateDBParameterGroupResponse' {dbParameterGroup} -> dbParameterGroup) (\s@CreateDBParameterGroupResponse' {} a -> s {dbParameterGroup = a} :: CreateDBParameterGroupResponse)

-- | The response's http status code.
createDBParameterGroupResponse_httpStatus :: Lens.Lens' CreateDBParameterGroupResponse Prelude.Int
createDBParameterGroupResponse_httpStatus = Lens.lens (\CreateDBParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDBParameterGroupResponse' {} a -> s {httpStatus = a} :: CreateDBParameterGroupResponse)

instance
  Prelude.NFData
    CreateDBParameterGroupResponse
  where
  rnf CreateDBParameterGroupResponse' {..} =
    Prelude.rnf dbParameterGroup
      `Prelude.seq` Prelude.rnf httpStatus
