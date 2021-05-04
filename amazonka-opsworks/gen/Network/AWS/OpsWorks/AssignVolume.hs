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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssignVolume' smart constructor.
data AssignVolume = AssignVolume'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The volume ID.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AssignVolume
newAssignVolume pVolumeId_ =
  AssignVolume'
    { instanceId = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | The instance ID.
assignVolume_instanceId :: Lens.Lens' AssignVolume (Prelude.Maybe Prelude.Text)
assignVolume_instanceId = Lens.lens (\AssignVolume' {instanceId} -> instanceId) (\s@AssignVolume' {} a -> s {instanceId = a} :: AssignVolume)

-- | The volume ID.
assignVolume_volumeId :: Lens.Lens' AssignVolume Prelude.Text
assignVolume_volumeId = Lens.lens (\AssignVolume' {volumeId} -> volumeId) (\s@AssignVolume' {} a -> s {volumeId = a} :: AssignVolume)

instance Prelude.AWSRequest AssignVolume where
  type Rs AssignVolume = AssignVolumeResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull AssignVolumeResponse'

instance Prelude.Hashable AssignVolume

instance Prelude.NFData AssignVolume

instance Prelude.ToHeaders AssignVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.AssignVolume" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssignVolume where
  toJSON AssignVolume' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InstanceId" Prelude..=) Prelude.<$> instanceId,
            Prelude.Just ("VolumeId" Prelude..= volumeId)
          ]
      )

instance Prelude.ToPath AssignVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssignVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssignVolumeResponse' smart constructor.
data AssignVolumeResponse = AssignVolumeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssignVolumeResponse ::
  AssignVolumeResponse
newAssignVolumeResponse = AssignVolumeResponse'

instance Prelude.NFData AssignVolumeResponse
