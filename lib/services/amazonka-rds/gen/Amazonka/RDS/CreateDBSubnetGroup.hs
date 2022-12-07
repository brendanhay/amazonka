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
-- Module      : Amazonka.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB subnet group. DB subnet groups must contain at least
-- one subnet in at least two AZs in the Amazon Web Services Region.
module Amazonka.RDS.CreateDBSubnetGroup
  ( -- * Creating a Request
    CreateDBSubnetGroup (..),
    newCreateDBSubnetGroup,

    -- * Request Lenses
    createDBSubnetGroup_tags,
    createDBSubnetGroup_dbSubnetGroupName,
    createDBSubnetGroup_dbSubnetGroupDescription,
    createDBSubnetGroup_subnetIds,

    -- * Destructuring the Response
    CreateDBSubnetGroupResponse (..),
    newCreateDBSubnetGroupResponse,

    -- * Response Lenses
    createDBSubnetGroupResponse_dbSubnetGroup,
    createDBSubnetGroupResponse_httpStatus,
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
-- /See:/ 'newCreateDBSubnetGroup' smart constructor.
data CreateDBSubnetGroup = CreateDBSubnetGroup'
  { -- | Tags to assign to the DB subnet group.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the DB subnet group. This value is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must contain no more than 255 letters, numbers, periods,
    --     underscores, spaces, or hyphens.
    --
    -- -   Must not be default.
    --
    -- -   First character must be a letter.
    --
    -- Example: @mydbsubnetgroup@
    dbSubnetGroupName :: Prelude.Text,
    -- | The description for the DB subnet group.
    dbSubnetGroupDescription :: Prelude.Text,
    -- | The EC2 Subnet IDs for the DB subnet group.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDBSubnetGroup_tags' - Tags to assign to the DB subnet group.
--
-- 'dbSubnetGroupName', 'createDBSubnetGroup_dbSubnetGroupName' - The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain no more than 255 letters, numbers, periods,
--     underscores, spaces, or hyphens.
--
-- -   Must not be default.
--
-- -   First character must be a letter.
--
-- Example: @mydbsubnetgroup@
--
-- 'dbSubnetGroupDescription', 'createDBSubnetGroup_dbSubnetGroupDescription' - The description for the DB subnet group.
--
-- 'subnetIds', 'createDBSubnetGroup_subnetIds' - The EC2 Subnet IDs for the DB subnet group.
newCreateDBSubnetGroup ::
  -- | 'dbSubnetGroupName'
  Prelude.Text ->
  -- | 'dbSubnetGroupDescription'
  Prelude.Text ->
  CreateDBSubnetGroup
newCreateDBSubnetGroup
  pDBSubnetGroupName_
  pDBSubnetGroupDescription_ =
    CreateDBSubnetGroup'
      { tags = Prelude.Nothing,
        dbSubnetGroupName = pDBSubnetGroupName_,
        dbSubnetGroupDescription =
          pDBSubnetGroupDescription_,
        subnetIds = Prelude.mempty
      }

-- | Tags to assign to the DB subnet group.
createDBSubnetGroup_tags :: Lens.Lens' CreateDBSubnetGroup (Prelude.Maybe [Tag])
createDBSubnetGroup_tags = Lens.lens (\CreateDBSubnetGroup' {tags} -> tags) (\s@CreateDBSubnetGroup' {} a -> s {tags = a} :: CreateDBSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain no more than 255 letters, numbers, periods,
--     underscores, spaces, or hyphens.
--
-- -   Must not be default.
--
-- -   First character must be a letter.
--
-- Example: @mydbsubnetgroup@
createDBSubnetGroup_dbSubnetGroupName :: Lens.Lens' CreateDBSubnetGroup Prelude.Text
createDBSubnetGroup_dbSubnetGroupName = Lens.lens (\CreateDBSubnetGroup' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBSubnetGroup' {} a -> s {dbSubnetGroupName = a} :: CreateDBSubnetGroup)

-- | The description for the DB subnet group.
createDBSubnetGroup_dbSubnetGroupDescription :: Lens.Lens' CreateDBSubnetGroup Prelude.Text
createDBSubnetGroup_dbSubnetGroupDescription = Lens.lens (\CreateDBSubnetGroup' {dbSubnetGroupDescription} -> dbSubnetGroupDescription) (\s@CreateDBSubnetGroup' {} a -> s {dbSubnetGroupDescription = a} :: CreateDBSubnetGroup)

-- | The EC2 Subnet IDs for the DB subnet group.
createDBSubnetGroup_subnetIds :: Lens.Lens' CreateDBSubnetGroup [Prelude.Text]
createDBSubnetGroup_subnetIds = Lens.lens (\CreateDBSubnetGroup' {subnetIds} -> subnetIds) (\s@CreateDBSubnetGroup' {} a -> s {subnetIds = a} :: CreateDBSubnetGroup) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDBSubnetGroup where
  type
    AWSResponse CreateDBSubnetGroup =
      CreateDBSubnetGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBSubnetGroupResult"
      ( \s h x ->
          CreateDBSubnetGroupResponse'
            Prelude.<$> (x Data..@? "DBSubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBSubnetGroup where
  hashWithSalt _salt CreateDBSubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` dbSubnetGroupDescription
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData CreateDBSubnetGroup where
  rnf CreateDBSubnetGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupDescription
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders CreateDBSubnetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBSubnetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBSubnetGroup where
  toQuery CreateDBSubnetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBSubnetGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DBSubnetGroupDescription"
          Data.=: dbSubnetGroupDescription,
        "SubnetIds"
          Data.=: Data.toQueryList "SubnetIdentifier" subnetIds
      ]

-- | /See:/ 'newCreateDBSubnetGroupResponse' smart constructor.
data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse'
  { dbSubnetGroup :: Prelude.Maybe DBSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroup', 'createDBSubnetGroupResponse_dbSubnetGroup' - Undocumented member.
--
-- 'httpStatus', 'createDBSubnetGroupResponse_httpStatus' - The response's http status code.
newCreateDBSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBSubnetGroupResponse
newCreateDBSubnetGroupResponse pHttpStatus_ =
  CreateDBSubnetGroupResponse'
    { dbSubnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBSubnetGroupResponse_dbSubnetGroup :: Lens.Lens' CreateDBSubnetGroupResponse (Prelude.Maybe DBSubnetGroup)
createDBSubnetGroupResponse_dbSubnetGroup = Lens.lens (\CreateDBSubnetGroupResponse' {dbSubnetGroup} -> dbSubnetGroup) (\s@CreateDBSubnetGroupResponse' {} a -> s {dbSubnetGroup = a} :: CreateDBSubnetGroupResponse)

-- | The response's http status code.
createDBSubnetGroupResponse_httpStatus :: Lens.Lens' CreateDBSubnetGroupResponse Prelude.Int
createDBSubnetGroupResponse_httpStatus = Lens.lens (\CreateDBSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDBSubnetGroupResponse' {} a -> s {httpStatus = a} :: CreateDBSubnetGroupResponse)

instance Prelude.NFData CreateDBSubnetGroupResponse where
  rnf CreateDBSubnetGroupResponse' {..} =
    Prelude.rnf dbSubnetGroup
      `Prelude.seq` Prelude.rnf httpStatus
