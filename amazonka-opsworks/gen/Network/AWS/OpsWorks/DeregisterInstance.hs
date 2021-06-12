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
-- Module      : Network.AWS.OpsWorks.DeregisterInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregister a registered Amazon EC2 or on-premises instance. This action
-- removes the instance from the stack and returns it to your control. This
-- action cannot be used with instances that were created with AWS OpsWorks
-- Stacks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DeregisterInstance
  ( -- * Creating a Request
    DeregisterInstance (..),
    newDeregisterInstance,

    -- * Request Lenses
    deregisterInstance_instanceId,

    -- * Destructuring the Response
    DeregisterInstanceResponse (..),
    newDeregisterInstanceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterInstance' smart constructor.
data DeregisterInstance = DeregisterInstance'
  { -- | The instance ID.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deregisterInstance_instanceId' - The instance ID.
newDeregisterInstance ::
  -- | 'instanceId'
  Core.Text ->
  DeregisterInstance
newDeregisterInstance pInstanceId_ =
  DeregisterInstance' {instanceId = pInstanceId_}

-- | The instance ID.
deregisterInstance_instanceId :: Lens.Lens' DeregisterInstance Core.Text
deregisterInstance_instanceId = Lens.lens (\DeregisterInstance' {instanceId} -> instanceId) (\s@DeregisterInstance' {} a -> s {instanceId = a} :: DeregisterInstance)

instance Core.AWSRequest DeregisterInstance where
  type
    AWSResponse DeregisterInstance =
      DeregisterInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeregisterInstanceResponse'

instance Core.Hashable DeregisterInstance

instance Core.NFData DeregisterInstance

instance Core.ToHeaders DeregisterInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DeregisterInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterInstance where
  toJSON DeregisterInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("InstanceId" Core..= instanceId)]
      )

instance Core.ToPath DeregisterInstance where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterInstanceResponse ::
  DeregisterInstanceResponse
newDeregisterInstanceResponse =
  DeregisterInstanceResponse'

instance Core.NFData DeregisterInstanceResponse
