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
-- Module      : Amazonka.OpsWorks.AssignVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorks.AssignVolume
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssignVolume' smart constructor.
data AssignVolume = AssignVolume'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The volume ID.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest AssignVolume where
  type AWSResponse AssignVolume = AssignVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull AssignVolumeResponse'

instance Prelude.Hashable AssignVolume where
  hashWithSalt _salt AssignVolume' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData AssignVolume where
  rnf AssignVolume' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf volumeId

instance Data.ToHeaders AssignVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.AssignVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssignVolume where
  toJSON AssignVolume' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceId" Data..=) Prelude.<$> instanceId,
            Prelude.Just ("VolumeId" Data..= volumeId)
          ]
      )

instance Data.ToPath AssignVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery AssignVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssignVolumeResponse' smart constructor.
data AssignVolumeResponse = AssignVolumeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssignVolumeResponse ::
  AssignVolumeResponse
newAssignVolumeResponse = AssignVolumeResponse'

instance Prelude.NFData AssignVolumeResponse where
  rnf _ = ()
