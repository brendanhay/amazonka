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
-- Module      : Amazonka.AutoScaling.DeleteAutoScalingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Auto Scaling group.
--
-- If the group has instances or scaling activities in progress, you must
-- specify the option to force the deletion in order for it to succeed. The
-- force delete operation will also terminate the EC2 instances. If the
-- group has a warm pool, the force delete option also deletes the warm
-- pool.
--
-- To remove instances from the Auto Scaling group before deleting it, call
-- the DetachInstances API with the list of instances and the option to
-- decrement the desired capacity. This ensures that Amazon EC2 Auto
-- Scaling does not launch replacement instances.
--
-- To terminate all instances before deleting the Auto Scaling group, call
-- the UpdateAutoScalingGroup API and set the minimum size and desired
-- capacity of the Auto Scaling group to zero.
--
-- If the group has scaling policies, deleting the group deletes the
-- policies, the underlying alarm actions, and any alarm that no longer has
-- an associated action.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-process-shutdown.html Delete your Auto Scaling infrastructure>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.DeleteAutoScalingGroup
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAutoScalingGroup' smart constructor.
data DeleteAutoScalingGroup = DeleteAutoScalingGroup'
  { -- | Specifies that the group is to be deleted along with all instances
    -- associated with the group, without waiting for all instances to be
    -- terminated. This action also deletes any outstanding lifecycle actions
    -- associated with the group.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- terminated. This action also deletes any outstanding lifecycle actions
-- associated with the group.
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
-- terminated. This action also deletes any outstanding lifecycle actions
-- associated with the group.
deleteAutoScalingGroup_forceDelete :: Lens.Lens' DeleteAutoScalingGroup (Prelude.Maybe Prelude.Bool)
deleteAutoScalingGroup_forceDelete = Lens.lens (\DeleteAutoScalingGroup' {forceDelete} -> forceDelete) (\s@DeleteAutoScalingGroup' {} a -> s {forceDelete = a} :: DeleteAutoScalingGroup)

-- | The name of the Auto Scaling group.
deleteAutoScalingGroup_autoScalingGroupName :: Lens.Lens' DeleteAutoScalingGroup Prelude.Text
deleteAutoScalingGroup_autoScalingGroupName = Lens.lens (\DeleteAutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@DeleteAutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: DeleteAutoScalingGroup)

instance Core.AWSRequest DeleteAutoScalingGroup where
  type
    AWSResponse DeleteAutoScalingGroup =
      DeleteAutoScalingGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAutoScalingGroupResponse'

instance Prelude.Hashable DeleteAutoScalingGroup where
  hashWithSalt _salt DeleteAutoScalingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DeleteAutoScalingGroup where
  rnf DeleteAutoScalingGroup' {..} =
    Prelude.rnf forceDelete
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders DeleteAutoScalingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAutoScalingGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAutoScalingGroup where
  toQuery DeleteAutoScalingGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteAutoScalingGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "ForceDelete" Data.=: forceDelete,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newDeleteAutoScalingGroupResponse' smart constructor.
data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
