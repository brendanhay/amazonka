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
-- Module      : Amazonka.OpsWorks.DeregisterVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon EBS volume. The volume can then be registered by
-- another stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DeregisterVolume
  ( -- * Creating a Request
    DeregisterVolume (..),
    newDeregisterVolume,

    -- * Request Lenses
    deregisterVolume_volumeId,

    -- * Destructuring the Response
    DeregisterVolumeResponse (..),
    newDeregisterVolumeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterVolume' smart constructor.
data DeregisterVolume = DeregisterVolume'
  { -- | The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks
    -- Stacks assigned to the instance when you registered the volume with the
    -- stack, not the Amazon EC2 volume ID.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeId', 'deregisterVolume_volumeId' - The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks
-- Stacks assigned to the instance when you registered the volume with the
-- stack, not the Amazon EC2 volume ID.
newDeregisterVolume ::
  -- | 'volumeId'
  Prelude.Text ->
  DeregisterVolume
newDeregisterVolume pVolumeId_ =
  DeregisterVolume' {volumeId = pVolumeId_}

-- | The AWS OpsWorks Stacks volume ID, which is the GUID that AWS OpsWorks
-- Stacks assigned to the instance when you registered the volume with the
-- stack, not the Amazon EC2 volume ID.
deregisterVolume_volumeId :: Lens.Lens' DeregisterVolume Prelude.Text
deregisterVolume_volumeId = Lens.lens (\DeregisterVolume' {volumeId} -> volumeId) (\s@DeregisterVolume' {} a -> s {volumeId = a} :: DeregisterVolume)

instance Core.AWSRequest DeregisterVolume where
  type
    AWSResponse DeregisterVolume =
      DeregisterVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeregisterVolumeResponse'

instance Prelude.Hashable DeregisterVolume where
  hashWithSalt _salt DeregisterVolume' {..} =
    _salt `Prelude.hashWithSalt` volumeId

instance Prelude.NFData DeregisterVolume where
  rnf DeregisterVolume' {..} = Prelude.rnf volumeId

instance Core.ToHeaders DeregisterVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DeregisterVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeregisterVolume where
  toJSON DeregisterVolume' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeId" Core..= volumeId)]
      )

instance Core.ToPath DeregisterVolume where
  toPath = Prelude.const "/"

instance Core.ToQuery DeregisterVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterVolumeResponse' smart constructor.
data DeregisterVolumeResponse = DeregisterVolumeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterVolumeResponse ::
  DeregisterVolumeResponse
newDeregisterVolumeResponse =
  DeregisterVolumeResponse'

instance Prelude.NFData DeregisterVolumeResponse where
  rnf _ = ()
