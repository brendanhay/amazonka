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
-- Module      : Network.AWS.OpsWorks.RebootInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a specified instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.RebootInstance
  ( -- * Creating a Request
    RebootInstance (..),
    newRebootInstance,

    -- * Request Lenses
    rebootInstance_instanceId,

    -- * Destructuring the Response
    RebootInstanceResponse (..),
    newRebootInstanceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebootInstance' smart constructor.
data RebootInstance = RebootInstance'
  { -- | The instance ID.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebootInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'rebootInstance_instanceId' - The instance ID.
newRebootInstance ::
  -- | 'instanceId'
  Core.Text ->
  RebootInstance
newRebootInstance pInstanceId_ =
  RebootInstance' {instanceId = pInstanceId_}

-- | The instance ID.
rebootInstance_instanceId :: Lens.Lens' RebootInstance Core.Text
rebootInstance_instanceId = Lens.lens (\RebootInstance' {instanceId} -> instanceId) (\s@RebootInstance' {} a -> s {instanceId = a} :: RebootInstance)

instance Core.AWSRequest RebootInstance where
  type
    AWSResponse RebootInstance =
      RebootInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RebootInstanceResponse'

instance Core.Hashable RebootInstance

instance Core.NFData RebootInstance

instance Core.ToHeaders RebootInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.RebootInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RebootInstance where
  toJSON RebootInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("InstanceId" Core..= instanceId)]
      )

instance Core.ToPath RebootInstance where
  toPath = Core.const "/"

instance Core.ToQuery RebootInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebootInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRebootInstanceResponse ::
  RebootInstanceResponse
newRebootInstanceResponse = RebootInstanceResponse'

instance Core.NFData RebootInstanceResponse
