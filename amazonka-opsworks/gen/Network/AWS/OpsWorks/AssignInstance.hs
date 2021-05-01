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
-- Module      : Network.AWS.OpsWorks.AssignInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assign a registered instance to a layer.
--
-- -   You can assign registered on-premises instances to any layer type.
--
-- -   You can assign registered Amazon EC2 instances only to custom
--     layers.
--
-- -   You cannot use this action with instances that were created with AWS
--     OpsWorks Stacks.
--
-- __Required Permissions__: To use this action, an AWS Identity and Access
-- Management (IAM) user must have a Manage permissions level for the stack
-- or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.AssignInstance
  ( -- * Creating a Request
    AssignInstance (..),
    newAssignInstance,

    -- * Request Lenses
    assignInstance_instanceId,
    assignInstance_layerIds,

    -- * Destructuring the Response
    AssignInstanceResponse (..),
    newAssignInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssignInstance' smart constructor.
data AssignInstance = AssignInstance'
  { -- | The instance ID.
    instanceId :: Prelude.Text,
    -- | The layer ID, which must correspond to a custom layer. You cannot assign
    -- a registered instance to a built-in layer.
    layerIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'assignInstance_instanceId' - The instance ID.
--
-- 'layerIds', 'assignInstance_layerIds' - The layer ID, which must correspond to a custom layer. You cannot assign
-- a registered instance to a built-in layer.
newAssignInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  AssignInstance
newAssignInstance pInstanceId_ =
  AssignInstance'
    { instanceId = pInstanceId_,
      layerIds = Prelude.mempty
    }

-- | The instance ID.
assignInstance_instanceId :: Lens.Lens' AssignInstance Prelude.Text
assignInstance_instanceId = Lens.lens (\AssignInstance' {instanceId} -> instanceId) (\s@AssignInstance' {} a -> s {instanceId = a} :: AssignInstance)

-- | The layer ID, which must correspond to a custom layer. You cannot assign
-- a registered instance to a built-in layer.
assignInstance_layerIds :: Lens.Lens' AssignInstance [Prelude.Text]
assignInstance_layerIds = Lens.lens (\AssignInstance' {layerIds} -> layerIds) (\s@AssignInstance' {} a -> s {layerIds = a} :: AssignInstance) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AssignInstance where
  type Rs AssignInstance = AssignInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AssignInstanceResponse'

instance Prelude.Hashable AssignInstance

instance Prelude.NFData AssignInstance

instance Prelude.ToHeaders AssignInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.AssignInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssignInstance where
  toJSON AssignInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Prelude..= instanceId),
            Prelude.Just ("LayerIds" Prelude..= layerIds)
          ]
      )

instance Prelude.ToPath AssignInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssignInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssignInstanceResponse' smart constructor.
data AssignInstanceResponse = AssignInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssignInstanceResponse ::
  AssignInstanceResponse
newAssignInstanceResponse = AssignInstanceResponse'

instance Prelude.NFData AssignInstanceResponse
