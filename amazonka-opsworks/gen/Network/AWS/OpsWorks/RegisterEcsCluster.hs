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
-- Module      : Network.AWS.OpsWorks.RegisterEcsCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a specified Amazon ECS cluster with a stack. You can register
-- only one cluster with a stack. A cluster can be registered with only one
-- stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.RegisterEcsCluster
  ( -- * Creating a Request
    RegisterEcsCluster (..),
    newRegisterEcsCluster,

    -- * Request Lenses
    registerEcsCluster_ecsClusterArn,
    registerEcsCluster_stackId,

    -- * Destructuring the Response
    RegisterEcsClusterResponse (..),
    newRegisterEcsClusterResponse,

    -- * Response Lenses
    registerEcsClusterResponse_ecsClusterArn,
    registerEcsClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterEcsCluster' smart constructor.
data RegisterEcsCluster = RegisterEcsCluster'
  { -- | The cluster\'s ARN.
    ecsClusterArn :: Core.Text,
    -- | The stack ID.
    stackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterEcsCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecsClusterArn', 'registerEcsCluster_ecsClusterArn' - The cluster\'s ARN.
--
-- 'stackId', 'registerEcsCluster_stackId' - The stack ID.
newRegisterEcsCluster ::
  -- | 'ecsClusterArn'
  Core.Text ->
  -- | 'stackId'
  Core.Text ->
  RegisterEcsCluster
newRegisterEcsCluster pEcsClusterArn_ pStackId_ =
  RegisterEcsCluster'
    { ecsClusterArn =
        pEcsClusterArn_,
      stackId = pStackId_
    }

-- | The cluster\'s ARN.
registerEcsCluster_ecsClusterArn :: Lens.Lens' RegisterEcsCluster Core.Text
registerEcsCluster_ecsClusterArn = Lens.lens (\RegisterEcsCluster' {ecsClusterArn} -> ecsClusterArn) (\s@RegisterEcsCluster' {} a -> s {ecsClusterArn = a} :: RegisterEcsCluster)

-- | The stack ID.
registerEcsCluster_stackId :: Lens.Lens' RegisterEcsCluster Core.Text
registerEcsCluster_stackId = Lens.lens (\RegisterEcsCluster' {stackId} -> stackId) (\s@RegisterEcsCluster' {} a -> s {stackId = a} :: RegisterEcsCluster)

instance Core.AWSRequest RegisterEcsCluster where
  type
    AWSResponse RegisterEcsCluster =
      RegisterEcsClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterEcsClusterResponse'
            Core.<$> (x Core..?> "EcsClusterArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterEcsCluster

instance Core.NFData RegisterEcsCluster

instance Core.ToHeaders RegisterEcsCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.RegisterEcsCluster" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterEcsCluster where
  toJSON RegisterEcsCluster' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EcsClusterArn" Core..= ecsClusterArn),
            Core.Just ("StackId" Core..= stackId)
          ]
      )

instance Core.ToPath RegisterEcsCluster where
  toPath = Core.const "/"

instance Core.ToQuery RegisterEcsCluster where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @RegisterEcsCluster@ request.
--
-- /See:/ 'newRegisterEcsClusterResponse' smart constructor.
data RegisterEcsClusterResponse = RegisterEcsClusterResponse'
  { -- | The cluster\'s ARN.
    ecsClusterArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterEcsClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecsClusterArn', 'registerEcsClusterResponse_ecsClusterArn' - The cluster\'s ARN.
--
-- 'httpStatus', 'registerEcsClusterResponse_httpStatus' - The response's http status code.
newRegisterEcsClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterEcsClusterResponse
newRegisterEcsClusterResponse pHttpStatus_ =
  RegisterEcsClusterResponse'
    { ecsClusterArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cluster\'s ARN.
registerEcsClusterResponse_ecsClusterArn :: Lens.Lens' RegisterEcsClusterResponse (Core.Maybe Core.Text)
registerEcsClusterResponse_ecsClusterArn = Lens.lens (\RegisterEcsClusterResponse' {ecsClusterArn} -> ecsClusterArn) (\s@RegisterEcsClusterResponse' {} a -> s {ecsClusterArn = a} :: RegisterEcsClusterResponse)

-- | The response's http status code.
registerEcsClusterResponse_httpStatus :: Lens.Lens' RegisterEcsClusterResponse Core.Int
registerEcsClusterResponse_httpStatus = Lens.lens (\RegisterEcsClusterResponse' {httpStatus} -> httpStatus) (\s@RegisterEcsClusterResponse' {} a -> s {httpStatus = a} :: RegisterEcsClusterResponse)

instance Core.NFData RegisterEcsClusterResponse
