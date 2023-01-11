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
-- Module      : Amazonka.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB security group.
--
-- The specified DB security group must not be associated with any DB
-- instances.
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
module Amazonka.RDS.DeleteDBSecurityGroup
  ( -- * Creating a Request
    DeleteDBSecurityGroup (..),
    newDeleteDBSecurityGroup,

    -- * Request Lenses
    deleteDBSecurityGroup_dbSecurityGroupName,

    -- * Destructuring the Response
    DeleteDBSecurityGroupResponse (..),
    newDeleteDBSecurityGroupResponse,
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
-- /See:/ 'newDeleteDBSecurityGroup' smart constructor.
data DeleteDBSecurityGroup = DeleteDBSecurityGroup'
  { -- | The name of the DB security group to delete.
    --
    -- You can\'t delete the default DB security group.
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
    dbSecurityGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroupName', 'deleteDBSecurityGroup_dbSecurityGroupName' - The name of the DB security group to delete.
--
-- You can\'t delete the default DB security group.
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
newDeleteDBSecurityGroup ::
  -- | 'dbSecurityGroupName'
  Prelude.Text ->
  DeleteDBSecurityGroup
newDeleteDBSecurityGroup pDBSecurityGroupName_ =
  DeleteDBSecurityGroup'
    { dbSecurityGroupName =
        pDBSecurityGroupName_
    }

-- | The name of the DB security group to delete.
--
-- You can\'t delete the default DB security group.
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
deleteDBSecurityGroup_dbSecurityGroupName :: Lens.Lens' DeleteDBSecurityGroup Prelude.Text
deleteDBSecurityGroup_dbSecurityGroupName = Lens.lens (\DeleteDBSecurityGroup' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@DeleteDBSecurityGroup' {} a -> s {dbSecurityGroupName = a} :: DeleteDBSecurityGroup)

instance Core.AWSRequest DeleteDBSecurityGroup where
  type
    AWSResponse DeleteDBSecurityGroup =
      DeleteDBSecurityGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteDBSecurityGroupResponse'

instance Prelude.Hashable DeleteDBSecurityGroup where
  hashWithSalt _salt DeleteDBSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` dbSecurityGroupName

instance Prelude.NFData DeleteDBSecurityGroup where
  rnf DeleteDBSecurityGroup' {..} =
    Prelude.rnf dbSecurityGroupName

instance Data.ToHeaders DeleteDBSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBSecurityGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBSecurityGroup where
  toQuery DeleteDBSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBSecurityGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBSecurityGroupName" Data.=: dbSecurityGroupName
      ]

-- | /See:/ 'newDeleteDBSecurityGroupResponse' smart constructor.
data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDBSecurityGroupResponse ::
  DeleteDBSecurityGroupResponse
newDeleteDBSecurityGroupResponse =
  DeleteDBSecurityGroupResponse'

instance Prelude.NFData DeleteDBSecurityGroupResponse where
  rnf _ = ()
