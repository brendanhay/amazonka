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
-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.DeleteSecurityGroup
  ( -- * Creating a Request
    DeleteSecurityGroup (..),
    newDeleteSecurityGroup,

    -- * Request Lenses
    deleteSecurityGroup_dryRun,
    deleteSecurityGroup_groupName,
    deleteSecurityGroup_groupId,

    -- * Destructuring the Response
    DeleteSecurityGroupResponse (..),
    newDeleteSecurityGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSecurityGroup' smart constructor.
data DeleteSecurityGroup = DeleteSecurityGroup'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | [EC2-Classic, default VPC] The name of the security group. You can
    -- specify either the security group name or the security group ID.
    groupName :: Core.Maybe Core.Text,
    -- | The ID of the security group. Required for a nondefault VPC.
    groupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'groupName', 'deleteSecurityGroup_groupName' - [EC2-Classic, default VPC] The name of the security group. You can
-- specify either the security group name or the security group ID.
--
-- 'groupId', 'deleteSecurityGroup_groupId' - The ID of the security group. Required for a nondefault VPC.
newDeleteSecurityGroup ::
  DeleteSecurityGroup
newDeleteSecurityGroup =
  DeleteSecurityGroup'
    { dryRun = Core.Nothing,
      groupName = Core.Nothing,
      groupId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteSecurityGroup_dryRun :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Core.Bool)
deleteSecurityGroup_dryRun = Lens.lens (\DeleteSecurityGroup' {dryRun} -> dryRun) (\s@DeleteSecurityGroup' {} a -> s {dryRun = a} :: DeleteSecurityGroup)

-- | [EC2-Classic, default VPC] The name of the security group. You can
-- specify either the security group name or the security group ID.
deleteSecurityGroup_groupName :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Core.Text)
deleteSecurityGroup_groupName = Lens.lens (\DeleteSecurityGroup' {groupName} -> groupName) (\s@DeleteSecurityGroup' {} a -> s {groupName = a} :: DeleteSecurityGroup)

-- | The ID of the security group. Required for a nondefault VPC.
deleteSecurityGroup_groupId :: Lens.Lens' DeleteSecurityGroup (Core.Maybe Core.Text)
deleteSecurityGroup_groupId = Lens.lens (\DeleteSecurityGroup' {groupId} -> groupId) (\s@DeleteSecurityGroup' {} a -> s {groupId = a} :: DeleteSecurityGroup)

instance Core.AWSRequest DeleteSecurityGroup where
  type
    AWSResponse DeleteSecurityGroup =
      DeleteSecurityGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteSecurityGroupResponse'

instance Core.Hashable DeleteSecurityGroup

instance Core.NFData DeleteSecurityGroup

instance Core.ToHeaders DeleteSecurityGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteSecurityGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSecurityGroup where
  toQuery DeleteSecurityGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteSecurityGroup" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "GroupName" Core.=: groupName,
        "GroupId" Core.=: groupId
      ]

-- | /See:/ 'newDeleteSecurityGroupResponse' smart constructor.
data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSecurityGroupResponse ::
  DeleteSecurityGroupResponse
newDeleteSecurityGroupResponse =
  DeleteSecurityGroupResponse'

instance Core.NFData DeleteSecurityGroupResponse
