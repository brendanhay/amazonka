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
-- Module      : Network.AWS.OpsWorks.UnassignInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns a registered instance from all layers that are using the
-- instance. The instance remains in the stack as an unassigned instance,
-- and can be assigned to another layer as needed. You cannot use this
-- action with instances that were created with AWS OpsWorks Stacks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information about user
-- permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UnassignInstance
  ( -- * Creating a Request
    UnassignInstance (..),
    newUnassignInstance,

    -- * Request Lenses
    unassignInstance_instanceId,

    -- * Destructuring the Response
    UnassignInstanceResponse (..),
    newUnassignInstanceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUnassignInstance' smart constructor.
data UnassignInstance = UnassignInstance'
  { -- | The instance ID.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnassignInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'unassignInstance_instanceId' - The instance ID.
newUnassignInstance ::
  -- | 'instanceId'
  Core.Text ->
  UnassignInstance
newUnassignInstance pInstanceId_ =
  UnassignInstance' {instanceId = pInstanceId_}

-- | The instance ID.
unassignInstance_instanceId :: Lens.Lens' UnassignInstance Core.Text
unassignInstance_instanceId = Lens.lens (\UnassignInstance' {instanceId} -> instanceId) (\s@UnassignInstance' {} a -> s {instanceId = a} :: UnassignInstance)

instance Core.AWSRequest UnassignInstance where
  type
    AWSResponse UnassignInstance =
      UnassignInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UnassignInstanceResponse'

instance Core.Hashable UnassignInstance

instance Core.NFData UnassignInstance

instance Core.ToHeaders UnassignInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UnassignInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UnassignInstance where
  toJSON UnassignInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("InstanceId" Core..= instanceId)]
      )

instance Core.ToPath UnassignInstance where
  toPath = Core.const "/"

instance Core.ToQuery UnassignInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUnassignInstanceResponse' smart constructor.
data UnassignInstanceResponse = UnassignInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnassignInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnassignInstanceResponse ::
  UnassignInstanceResponse
newUnassignInstanceResponse =
  UnassignInstanceResponse'

instance Core.NFData UnassignInstanceResponse
