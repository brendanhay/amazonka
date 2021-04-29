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
-- Module      : Network.AWS.OpsWorks.DeleteInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified instance, which terminates the associated Amazon EC2
-- instance. You must stop an instance before you can delete it.
--
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-delete.html Deleting Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DeleteInstance
  ( -- * Creating a Request
    DeleteInstance (..),
    newDeleteInstance,

    -- * Request Lenses
    deleteInstance_deleteVolumes,
    deleteInstance_deleteElasticIp,
    deleteInstance_instanceId,

    -- * Destructuring the Response
    DeleteInstanceResponse (..),
    newDeleteInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { -- | Whether to delete the instance\'s Amazon EBS volumes.
    deleteVolumes :: Prelude.Maybe Prelude.Bool,
    -- | Whether to delete the instance Elastic IP address.
    deleteElasticIp :: Prelude.Maybe Prelude.Bool,
    -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteVolumes', 'deleteInstance_deleteVolumes' - Whether to delete the instance\'s Amazon EBS volumes.
--
-- 'deleteElasticIp', 'deleteInstance_deleteElasticIp' - Whether to delete the instance Elastic IP address.
--
-- 'instanceId', 'deleteInstance_instanceId' - The instance ID.
newDeleteInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  DeleteInstance
newDeleteInstance pInstanceId_ =
  DeleteInstance'
    { deleteVolumes = Prelude.Nothing,
      deleteElasticIp = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Whether to delete the instance\'s Amazon EBS volumes.
deleteInstance_deleteVolumes :: Lens.Lens' DeleteInstance (Prelude.Maybe Prelude.Bool)
deleteInstance_deleteVolumes = Lens.lens (\DeleteInstance' {deleteVolumes} -> deleteVolumes) (\s@DeleteInstance' {} a -> s {deleteVolumes = a} :: DeleteInstance)

-- | Whether to delete the instance Elastic IP address.
deleteInstance_deleteElasticIp :: Lens.Lens' DeleteInstance (Prelude.Maybe Prelude.Bool)
deleteInstance_deleteElasticIp = Lens.lens (\DeleteInstance' {deleteElasticIp} -> deleteElasticIp) (\s@DeleteInstance' {} a -> s {deleteElasticIp = a} :: DeleteInstance)

-- | The instance ID.
deleteInstance_instanceId :: Lens.Lens' DeleteInstance Prelude.Text
deleteInstance_instanceId = Lens.lens (\DeleteInstance' {instanceId} -> instanceId) (\s@DeleteInstance' {} a -> s {instanceId = a} :: DeleteInstance)

instance Prelude.AWSRequest DeleteInstance where
  type Rs DeleteInstance = DeleteInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteInstanceResponse'

instance Prelude.Hashable DeleteInstance

instance Prelude.NFData DeleteInstance

instance Prelude.ToHeaders DeleteInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DeleteInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteInstance where
  toJSON DeleteInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeleteVolumes" Prelude..=)
              Prelude.<$> deleteVolumes,
            ("DeleteElasticIp" Prelude..=)
              Prelude.<$> deleteElasticIp,
            Prelude.Just ("InstanceId" Prelude..= instanceId)
          ]
      )

instance Prelude.ToPath DeleteInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInstanceResponse ::
  DeleteInstanceResponse
newDeleteInstanceResponse = DeleteInstanceResponse'

instance Prelude.NFData DeleteInstanceResponse
