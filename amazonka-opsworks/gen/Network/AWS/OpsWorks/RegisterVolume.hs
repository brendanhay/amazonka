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
-- Module      : Network.AWS.OpsWorks.RegisterVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon EBS volume with a specified stack. A volume can be
-- registered with only one stack at a time. If the volume is already
-- registered, you must first deregister it by calling DeregisterVolume.
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.RegisterVolume
  ( -- * Creating a Request
    RegisterVolume (..),
    newRegisterVolume,

    -- * Request Lenses
    registerVolume_ec2VolumeId,
    registerVolume_stackId,

    -- * Destructuring the Response
    RegisterVolumeResponse (..),
    newRegisterVolumeResponse,

    -- * Response Lenses
    registerVolumeResponse_volumeId,
    registerVolumeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterVolume' smart constructor.
data RegisterVolume = RegisterVolume'
  { -- | The Amazon EBS volume ID.
    ec2VolumeId :: Core.Maybe Core.Text,
    -- | The stack ID.
    stackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2VolumeId', 'registerVolume_ec2VolumeId' - The Amazon EBS volume ID.
--
-- 'stackId', 'registerVolume_stackId' - The stack ID.
newRegisterVolume ::
  -- | 'stackId'
  Core.Text ->
  RegisterVolume
newRegisterVolume pStackId_ =
  RegisterVolume'
    { ec2VolumeId = Core.Nothing,
      stackId = pStackId_
    }

-- | The Amazon EBS volume ID.
registerVolume_ec2VolumeId :: Lens.Lens' RegisterVolume (Core.Maybe Core.Text)
registerVolume_ec2VolumeId = Lens.lens (\RegisterVolume' {ec2VolumeId} -> ec2VolumeId) (\s@RegisterVolume' {} a -> s {ec2VolumeId = a} :: RegisterVolume)

-- | The stack ID.
registerVolume_stackId :: Lens.Lens' RegisterVolume Core.Text
registerVolume_stackId = Lens.lens (\RegisterVolume' {stackId} -> stackId) (\s@RegisterVolume' {} a -> s {stackId = a} :: RegisterVolume)

instance Core.AWSRequest RegisterVolume where
  type
    AWSResponse RegisterVolume =
      RegisterVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterVolumeResponse'
            Core.<$> (x Core..?> "VolumeId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterVolume

instance Core.NFData RegisterVolume

instance Core.ToHeaders RegisterVolume where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.RegisterVolume" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterVolume where
  toJSON RegisterVolume' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Ec2VolumeId" Core..=) Core.<$> ec2VolumeId,
            Core.Just ("StackId" Core..= stackId)
          ]
      )

instance Core.ToPath RegisterVolume where
  toPath = Core.const "/"

instance Core.ToQuery RegisterVolume where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @RegisterVolume@ request.
--
-- /See:/ 'newRegisterVolumeResponse' smart constructor.
data RegisterVolumeResponse = RegisterVolumeResponse'
  { -- | The volume ID.
    volumeId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeId', 'registerVolumeResponse_volumeId' - The volume ID.
--
-- 'httpStatus', 'registerVolumeResponse_httpStatus' - The response's http status code.
newRegisterVolumeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterVolumeResponse
newRegisterVolumeResponse pHttpStatus_ =
  RegisterVolumeResponse'
    { volumeId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The volume ID.
registerVolumeResponse_volumeId :: Lens.Lens' RegisterVolumeResponse (Core.Maybe Core.Text)
registerVolumeResponse_volumeId = Lens.lens (\RegisterVolumeResponse' {volumeId} -> volumeId) (\s@RegisterVolumeResponse' {} a -> s {volumeId = a} :: RegisterVolumeResponse)

-- | The response's http status code.
registerVolumeResponse_httpStatus :: Lens.Lens' RegisterVolumeResponse Core.Int
registerVolumeResponse_httpStatus = Lens.lens (\RegisterVolumeResponse' {httpStatus} -> httpStatus) (\s@RegisterVolumeResponse' {} a -> s {httpStatus = a} :: RegisterVolumeResponse)

instance Core.NFData RegisterVolumeResponse
