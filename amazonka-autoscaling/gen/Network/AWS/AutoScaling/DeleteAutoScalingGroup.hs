{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAutoScalingGroup' smart constructor.
data DeleteAutoScalingGroup = DeleteAutoScalingGroup'
  { -- | Specifies that the group is to be deleted along with all instances
    -- associated with the group, without waiting for all instances to be
    -- terminated. This parameter also deletes any lifecycle actions associated
    -- with the group.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAutoScalingGroup
newDeleteAutoScalingGroup pAutoScalingGroupName_ =
  DeleteAutoScalingGroup'
    { forceDelete =
        Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Specifies that the group is to be deleted along with all instances
-- associated with the group, without waiting for all instances to be
-- terminated. This parameter also deletes any lifecycle actions associated
-- with the group.
deleteAutoScalingGroup_forceDelete :: Lens.Lens' DeleteAutoScalingGroup (Prelude.Maybe Prelude.Bool)
deleteAutoScalingGroup_forceDelete = Lens.lens (\DeleteAutoScalingGroup' {forceDelete} -> forceDelete) (\s@DeleteAutoScalingGroup' {} a -> s {forceDelete = a} :: DeleteAutoScalingGroup)

-- | The name of the Auto Scaling group.
deleteAutoScalingGroup_autoScalingGroupName :: Lens.Lens' DeleteAutoScalingGroup Prelude.Text
deleteAutoScalingGroup_autoScalingGroupName = Lens.lens (\DeleteAutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@DeleteAutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: DeleteAutoScalingGroup)

instance Prelude.AWSRequest DeleteAutoScalingGroup where
  type
    Rs DeleteAutoScalingGroup =
      DeleteAutoScalingGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteAutoScalingGroupResponse'

instance Prelude.Hashable DeleteAutoScalingGroup

instance Prelude.NFData DeleteAutoScalingGroup

instance Prelude.ToHeaders DeleteAutoScalingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteAutoScalingGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAutoScalingGroup where
  toQuery DeleteAutoScalingGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteAutoScalingGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "ForceDelete" Prelude.=: forceDelete,
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName
      ]

-- | /See:/ 'newDeleteAutoScalingGroupResponse' smart constructor.
data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAutoScalingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAutoScalingGroupResponse ::
  DeleteAutoScalingGroupResponse
newDeleteAutoScalingGroupResponse =
  DeleteAutoScalingGroupResponse'

instance
  Prelude.NFData
    DeleteAutoScalingGroupResponse
