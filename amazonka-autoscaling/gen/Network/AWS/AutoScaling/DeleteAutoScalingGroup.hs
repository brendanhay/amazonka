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
-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Auto Scaling group.
--
-- If the group has instances or scaling activities in progress, you must
-- specify the option to force the deletion in order for it to succeed.
--
-- If the group has policies, deleting the group deletes the policies, the
-- underlying alarm actions, and any alarm that no longer has an associated
-- action.
--
-- To remove instances from the Auto Scaling group before deleting it, call
-- the DetachInstances API with the list of instances and the option to
-- decrement the desired capacity. This ensures that Amazon EC2 Auto
-- Scaling does not launch replacement instances.
--
-- To terminate all instances before deleting the Auto Scaling group, call
-- the UpdateAutoScalingGroup API and set the minimum size and desired
-- capacity of the Auto Scaling group to zero.
module Network.AWS.AutoScaling.DeleteAutoScalingGroup
  ( -- * Creating a Request
    DeleteAutoScalingGroup (..),
    newDeleteAutoScalingGroup,

    -- * Request Lenses
    deleteAutoScalingGroup_forceDelete,
    deleteAutoScalingGroup_autoScalingGroupName,

    -- * Destructuring the Response
    DeleteAutoScalingGroupResponse (..),
    newDeleteAutoScalingGroupResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAutoScalingGroup' smart constructor.
data DeleteAutoScalingGroup = DeleteAutoScalingGroup'
  { -- | Specifies that the group is to be deleted along with all instances
    -- associated with the group, without waiting for all instances to be
    -- terminated. This parameter also deletes any lifecycle actions associated
    -- with the group.
    forceDelete :: Core.Maybe Core.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDelete', 'deleteAutoScalingGroup_forceDelete' - Specifies that the group is to be deleted along with all instances
-- associated with the group, without waiting for all instances to be
-- terminated. This parameter also deletes any lifecycle actions associated
-- with the group.
--
-- 'autoScalingGroupName', 'deleteAutoScalingGroup_autoScalingGroupName' - The name of the Auto Scaling group.
newDeleteAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  DeleteAutoScalingGroup
newDeleteAutoScalingGroup pAutoScalingGroupName_ =
  DeleteAutoScalingGroup'
    { forceDelete = Core.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Specifies that the group is to be deleted along with all instances
-- associated with the group, without waiting for all instances to be
-- terminated. This parameter also deletes any lifecycle actions associated
-- with the group.
deleteAutoScalingGroup_forceDelete :: Lens.Lens' DeleteAutoScalingGroup (Core.Maybe Core.Bool)
deleteAutoScalingGroup_forceDelete = Lens.lens (\DeleteAutoScalingGroup' {forceDelete} -> forceDelete) (\s@DeleteAutoScalingGroup' {} a -> s {forceDelete = a} :: DeleteAutoScalingGroup)

-- | The name of the Auto Scaling group.
deleteAutoScalingGroup_autoScalingGroupName :: Lens.Lens' DeleteAutoScalingGroup Core.Text
deleteAutoScalingGroup_autoScalingGroupName = Lens.lens (\DeleteAutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@DeleteAutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: DeleteAutoScalingGroup)

instance Core.AWSRequest DeleteAutoScalingGroup where
  type
    AWSResponse DeleteAutoScalingGroup =
      DeleteAutoScalingGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteAutoScalingGroupResponse'

instance Core.Hashable DeleteAutoScalingGroup

instance Core.NFData DeleteAutoScalingGroup

instance Core.ToHeaders DeleteAutoScalingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteAutoScalingGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAutoScalingGroup where
  toQuery DeleteAutoScalingGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteAutoScalingGroup" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "ForceDelete" Core.=: forceDelete,
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDeleteAutoScalingGroupResponse' smart constructor.
data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAutoScalingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAutoScalingGroupResponse ::
  DeleteAutoScalingGroupResponse
newDeleteAutoScalingGroupResponse =
  DeleteAutoScalingGroupResponse'

instance Core.NFData DeleteAutoScalingGroupResponse
