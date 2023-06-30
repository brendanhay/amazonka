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
-- Module      : Amazonka.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security group.
--
-- If you attempt to delete a security group that is associated with an
-- instance, or is referenced by another security group, the operation
-- fails with @InvalidGroup.InUse@ in EC2-Classic or @DependencyViolation@
-- in EC2-VPC.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DeleteSecurityGroup
  ( -- * Creating a Request
    DeleteSecurityGroup (..),
    newDeleteSecurityGroup,

    -- * Request Lenses
    deleteSecurityGroup_dryRun,
    deleteSecurityGroup_groupId,
    deleteSecurityGroup_groupName,

    -- * Destructuring the Response
    DeleteSecurityGroupResponse (..),
    newDeleteSecurityGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSecurityGroup' smart constructor.
data DeleteSecurityGroup = DeleteSecurityGroup'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the security group. Required for a nondefault VPC.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic, default VPC] The name of the security group. You can
    -- specify either the security group name or the security group ID. For
    -- security groups in a nondefault VPC, you must specify the security group
    -- ID.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteSecurityGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupId', 'deleteSecurityGroup_groupId' - The ID of the security group. Required for a nondefault VPC.
--
-- 'groupName', 'deleteSecurityGroup_groupName' - [EC2-Classic, default VPC] The name of the security group. You can
-- specify either the security group name or the security group ID. For
-- security groups in a nondefault VPC, you must specify the security group
-- ID.
newDeleteSecurityGroup ::
  DeleteSecurityGroup
newDeleteSecurityGroup =
  DeleteSecurityGroup'
    { dryRun = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteSecurityGroup_dryRun :: Lens.Lens' DeleteSecurityGroup (Prelude.Maybe Prelude.Bool)
deleteSecurityGroup_dryRun = Lens.lens (\DeleteSecurityGroup' {dryRun} -> dryRun) (\s@DeleteSecurityGroup' {} a -> s {dryRun = a} :: DeleteSecurityGroup)

-- | The ID of the security group. Required for a nondefault VPC.
deleteSecurityGroup_groupId :: Lens.Lens' DeleteSecurityGroup (Prelude.Maybe Prelude.Text)
deleteSecurityGroup_groupId = Lens.lens (\DeleteSecurityGroup' {groupId} -> groupId) (\s@DeleteSecurityGroup' {} a -> s {groupId = a} :: DeleteSecurityGroup)

-- | [EC2-Classic, default VPC] The name of the security group. You can
-- specify either the security group name or the security group ID. For
-- security groups in a nondefault VPC, you must specify the security group
-- ID.
deleteSecurityGroup_groupName :: Lens.Lens' DeleteSecurityGroup (Prelude.Maybe Prelude.Text)
deleteSecurityGroup_groupName = Lens.lens (\DeleteSecurityGroup' {groupName} -> groupName) (\s@DeleteSecurityGroup' {} a -> s {groupName = a} :: DeleteSecurityGroup)

instance Core.AWSRequest DeleteSecurityGroup where
  type
    AWSResponse DeleteSecurityGroup =
      DeleteSecurityGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteSecurityGroupResponse'

instance Prelude.Hashable DeleteSecurityGroup where
  hashWithSalt _salt DeleteSecurityGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData DeleteSecurityGroup where
  rnf DeleteSecurityGroup' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName

instance Data.ToHeaders DeleteSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSecurityGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSecurityGroup where
  toQuery DeleteSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteSecurityGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "GroupId" Data.=: groupId,
        "GroupName" Data.=: groupName
      ]

-- | /See:/ 'newDeleteSecurityGroupResponse' smart constructor.
data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSecurityGroupResponse ::
  DeleteSecurityGroupResponse
newDeleteSecurityGroupResponse =
  DeleteSecurityGroupResponse'

instance Prelude.NFData DeleteSecurityGroupResponse where
  rnf _ = ()
