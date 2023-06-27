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
-- Module      : Amazonka.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB security group. DB security groups control access to a
-- DB instance.
--
-- A DB security group controls access to EC2-Classic DB instances that are
-- not in a VPC.
--
-- EC2-Classic was retired on August 15, 2022. If you haven\'t migrated
-- from EC2-Classic to a VPC, we recommend that you migrate as soon as
-- possible. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon EC2 User Guide/, the blog
-- <http://aws.amazon.com/blogs/aws/ec2-classic-is-retiring-heres-how-to-prepare/ EC2-Classic Networking is Retiring – Here’s How to Prepare>,
-- and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.Non-VPC2VPC.html Moving a DB instance not in a VPC into a VPC>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.CreateDBSecurityGroup
  ( -- * Creating a Request
    CreateDBSecurityGroup (..),
    newCreateDBSecurityGroup,

    -- * Request Lenses
    createDBSecurityGroup_tags,
    createDBSecurityGroup_dbSecurityGroupName,
    createDBSecurityGroup_dbSecurityGroupDescription,

    -- * Destructuring the Response
    CreateDBSecurityGroupResponse (..),
    newCreateDBSecurityGroupResponse,

    -- * Response Lenses
    createDBSecurityGroupResponse_dbSecurityGroup,
    createDBSecurityGroupResponse_httpStatus,
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
-- /See:/ 'newCreateDBSecurityGroup' smart constructor.
data CreateDBSecurityGroup = CreateDBSecurityGroup'
  { -- | Tags to assign to the DB security group.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the DB security group. This value is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- -   Must not be \"Default\"
    --
    -- Example: @mysecuritygroup@
    dbSecurityGroupName :: Prelude.Text,
    -- | The description for the DB security group.
    dbSecurityGroupDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDBSecurityGroup_tags' - Tags to assign to the DB security group.
--
-- 'dbSecurityGroupName', 'createDBSecurityGroup_dbSecurityGroupName' - The name for the DB security group. This value is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- -   Must not be \"Default\"
--
-- Example: @mysecuritygroup@
--
-- 'dbSecurityGroupDescription', 'createDBSecurityGroup_dbSecurityGroupDescription' - The description for the DB security group.
newCreateDBSecurityGroup ::
  -- | 'dbSecurityGroupName'
  Prelude.Text ->
  -- | 'dbSecurityGroupDescription'
  Prelude.Text ->
  CreateDBSecurityGroup
newCreateDBSecurityGroup
  pDBSecurityGroupName_
  pDBSecurityGroupDescription_ =
    CreateDBSecurityGroup'
      { tags = Prelude.Nothing,
        dbSecurityGroupName = pDBSecurityGroupName_,
        dbSecurityGroupDescription =
          pDBSecurityGroupDescription_
      }

-- | Tags to assign to the DB security group.
createDBSecurityGroup_tags :: Lens.Lens' CreateDBSecurityGroup (Prelude.Maybe [Tag])
createDBSecurityGroup_tags = Lens.lens (\CreateDBSecurityGroup' {tags} -> tags) (\s@CreateDBSecurityGroup' {} a -> s {tags = a} :: CreateDBSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name for the DB security group. This value is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- -   Must not be \"Default\"
--
-- Example: @mysecuritygroup@
createDBSecurityGroup_dbSecurityGroupName :: Lens.Lens' CreateDBSecurityGroup Prelude.Text
createDBSecurityGroup_dbSecurityGroupName = Lens.lens (\CreateDBSecurityGroup' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@CreateDBSecurityGroup' {} a -> s {dbSecurityGroupName = a} :: CreateDBSecurityGroup)

-- | The description for the DB security group.
createDBSecurityGroup_dbSecurityGroupDescription :: Lens.Lens' CreateDBSecurityGroup Prelude.Text
createDBSecurityGroup_dbSecurityGroupDescription = Lens.lens (\CreateDBSecurityGroup' {dbSecurityGroupDescription} -> dbSecurityGroupDescription) (\s@CreateDBSecurityGroup' {} a -> s {dbSecurityGroupDescription = a} :: CreateDBSecurityGroup)

instance Core.AWSRequest CreateDBSecurityGroup where
  type
    AWSResponse CreateDBSecurityGroup =
      CreateDBSecurityGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBSecurityGroupResult"
      ( \s h x ->
          CreateDBSecurityGroupResponse'
            Prelude.<$> (x Data..@? "DBSecurityGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBSecurityGroup where
  hashWithSalt _salt CreateDBSecurityGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dbSecurityGroupName
      `Prelude.hashWithSalt` dbSecurityGroupDescription

instance Prelude.NFData CreateDBSecurityGroup where
  rnf CreateDBSecurityGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dbSecurityGroupName
      `Prelude.seq` Prelude.rnf dbSecurityGroupDescription

instance Data.ToHeaders CreateDBSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBSecurityGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBSecurityGroup where
  toQuery CreateDBSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBSecurityGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "DBSecurityGroupName" Data.=: dbSecurityGroupName,
        "DBSecurityGroupDescription"
          Data.=: dbSecurityGroupDescription
      ]

-- | /See:/ 'newCreateDBSecurityGroupResponse' smart constructor.
data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse'
  { dbSecurityGroup :: Prelude.Maybe DBSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroup', 'createDBSecurityGroupResponse_dbSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'createDBSecurityGroupResponse_httpStatus' - The response's http status code.
newCreateDBSecurityGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBSecurityGroupResponse
newCreateDBSecurityGroupResponse pHttpStatus_ =
  CreateDBSecurityGroupResponse'
    { dbSecurityGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBSecurityGroupResponse_dbSecurityGroup :: Lens.Lens' CreateDBSecurityGroupResponse (Prelude.Maybe DBSecurityGroup)
createDBSecurityGroupResponse_dbSecurityGroup = Lens.lens (\CreateDBSecurityGroupResponse' {dbSecurityGroup} -> dbSecurityGroup) (\s@CreateDBSecurityGroupResponse' {} a -> s {dbSecurityGroup = a} :: CreateDBSecurityGroupResponse)

-- | The response's http status code.
createDBSecurityGroupResponse_httpStatus :: Lens.Lens' CreateDBSecurityGroupResponse Prelude.Int
createDBSecurityGroupResponse_httpStatus = Lens.lens (\CreateDBSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDBSecurityGroupResponse' {} a -> s {httpStatus = a} :: CreateDBSecurityGroupResponse)

instance Prelude.NFData CreateDBSecurityGroupResponse where
  rnf CreateDBSecurityGroupResponse' {..} =
    Prelude.rnf dbSecurityGroup
      `Prelude.seq` Prelude.rnf httpStatus
