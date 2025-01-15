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
-- Module      : Amazonka.OpsWorks.DeleteInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.OpsWorks.DeleteInstance
  ( -- * Creating a Request
    DeleteInstance (..),
    newDeleteInstance,

    -- * Request Lenses
    deleteInstance_deleteElasticIp,
    deleteInstance_deleteVolumes,
    deleteInstance_instanceId,

    -- * Destructuring the Response
    DeleteInstanceResponse (..),
    newDeleteInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { -- | Whether to delete the instance Elastic IP address.
    deleteElasticIp :: Prelude.Maybe Prelude.Bool,
    -- | Whether to delete the instance\'s Amazon EBS volumes.
    deleteVolumes :: Prelude.Maybe Prelude.Bool,
    -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteElasticIp', 'deleteInstance_deleteElasticIp' - Whether to delete the instance Elastic IP address.
--
-- 'deleteVolumes', 'deleteInstance_deleteVolumes' - Whether to delete the instance\'s Amazon EBS volumes.
--
-- 'instanceId', 'deleteInstance_instanceId' - The instance ID.
newDeleteInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  DeleteInstance
newDeleteInstance pInstanceId_ =
  DeleteInstance'
    { deleteElasticIp = Prelude.Nothing,
      deleteVolumes = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Whether to delete the instance Elastic IP address.
deleteInstance_deleteElasticIp :: Lens.Lens' DeleteInstance (Prelude.Maybe Prelude.Bool)
deleteInstance_deleteElasticIp = Lens.lens (\DeleteInstance' {deleteElasticIp} -> deleteElasticIp) (\s@DeleteInstance' {} a -> s {deleteElasticIp = a} :: DeleteInstance)

-- | Whether to delete the instance\'s Amazon EBS volumes.
deleteInstance_deleteVolumes :: Lens.Lens' DeleteInstance (Prelude.Maybe Prelude.Bool)
deleteInstance_deleteVolumes = Lens.lens (\DeleteInstance' {deleteVolumes} -> deleteVolumes) (\s@DeleteInstance' {} a -> s {deleteVolumes = a} :: DeleteInstance)

-- | The instance ID.
deleteInstance_instanceId :: Lens.Lens' DeleteInstance Prelude.Text
deleteInstance_instanceId = Lens.lens (\DeleteInstance' {instanceId} -> instanceId) (\s@DeleteInstance' {} a -> s {instanceId = a} :: DeleteInstance)

instance Core.AWSRequest DeleteInstance where
  type
    AWSResponse DeleteInstance =
      DeleteInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteInstanceResponse'

instance Prelude.Hashable DeleteInstance where
  hashWithSalt _salt DeleteInstance' {..} =
    _salt
      `Prelude.hashWithSalt` deleteElasticIp
      `Prelude.hashWithSalt` deleteVolumes
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DeleteInstance where
  rnf DeleteInstance' {..} =
    Prelude.rnf deleteElasticIp `Prelude.seq`
      Prelude.rnf deleteVolumes `Prelude.seq`
        Prelude.rnf instanceId

instance Data.ToHeaders DeleteInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DeleteInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteInstance where
  toJSON DeleteInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteElasticIp" Data..=)
              Prelude.<$> deleteElasticIp,
            ("DeleteVolumes" Data..=) Prelude.<$> deleteVolumes,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath DeleteInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInstanceResponse ::
  DeleteInstanceResponse
newDeleteInstanceResponse = DeleteInstanceResponse'

instance Prelude.NFData DeleteInstanceResponse where
  rnf _ = ()
