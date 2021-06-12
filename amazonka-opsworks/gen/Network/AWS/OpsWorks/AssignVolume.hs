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
-- Module      : Network.AWS.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one of the stack\'s registered Amazon EBS volumes to a specified
-- instance. The volume must first be registered with the stack by calling
-- RegisterVolume. After you register the volume, you must call
-- UpdateVolume to specify a mount point before calling @AssignVolume@. For
-- more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.AssignVolume
  ( -- * Creating a Request
    AssignVolume (..),
    newAssignVolume,

    -- * Request Lenses
    assignVolume_instanceId,
    assignVolume_volumeId,

    -- * Destructuring the Response
    AssignVolumeResponse (..),
    newAssignVolumeResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssignVolume' smart constructor.
data AssignVolume = AssignVolume'
  { -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The volume ID.
    volumeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssignVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'assignVolume_instanceId' - The instance ID.
--
-- 'volumeId', 'assignVolume_volumeId' - The volume ID.
newAssignVolume ::
  -- | 'volumeId'
  Core.Text ->
  AssignVolume
newAssignVolume pVolumeId_ =
  AssignVolume'
    { instanceId = Core.Nothing,
      volumeId = pVolumeId_
    }

-- | The instance ID.
assignVolume_instanceId :: Lens.Lens' AssignVolume (Core.Maybe Core.Text)
assignVolume_instanceId = Lens.lens (\AssignVolume' {instanceId} -> instanceId) (\s@AssignVolume' {} a -> s {instanceId = a} :: AssignVolume)

-- | The volume ID.
assignVolume_volumeId :: Lens.Lens' AssignVolume Core.Text
assignVolume_volumeId = Lens.lens (\AssignVolume' {volumeId} -> volumeId) (\s@AssignVolume' {} a -> s {volumeId = a} :: AssignVolume)

instance Core.AWSRequest AssignVolume where
  type AWSResponse AssignVolume = AssignVolumeResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull AssignVolumeResponse'

instance Core.Hashable AssignVolume

instance Core.NFData AssignVolume

instance Core.ToHeaders AssignVolume where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.AssignVolume" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssignVolume where
  toJSON AssignVolume' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            Core.Just ("VolumeId" Core..= volumeId)
          ]
      )

instance Core.ToPath AssignVolume where
  toPath = Core.const "/"

instance Core.ToQuery AssignVolume where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssignVolumeResponse' smart constructor.
data AssignVolumeResponse = AssignVolumeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssignVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssignVolumeResponse ::
  AssignVolumeResponse
newAssignVolumeResponse = AssignVolumeResponse'

instance Core.NFData AssignVolumeResponse
