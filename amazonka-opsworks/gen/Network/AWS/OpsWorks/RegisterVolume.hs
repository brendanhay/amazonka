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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterVolume' smart constructor.
data RegisterVolume = RegisterVolume'
  { -- | The Amazon EBS volume ID.
    ec2VolumeId :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  RegisterVolume
newRegisterVolume pStackId_ =
  RegisterVolume'
    { ec2VolumeId = Prelude.Nothing,
      stackId = pStackId_
    }

-- | The Amazon EBS volume ID.
registerVolume_ec2VolumeId :: Lens.Lens' RegisterVolume (Prelude.Maybe Prelude.Text)
registerVolume_ec2VolumeId = Lens.lens (\RegisterVolume' {ec2VolumeId} -> ec2VolumeId) (\s@RegisterVolume' {} a -> s {ec2VolumeId = a} :: RegisterVolume)

-- | The stack ID.
registerVolume_stackId :: Lens.Lens' RegisterVolume Prelude.Text
registerVolume_stackId = Lens.lens (\RegisterVolume' {stackId} -> stackId) (\s@RegisterVolume' {} a -> s {stackId = a} :: RegisterVolume)

instance Prelude.AWSRequest RegisterVolume where
  type Rs RegisterVolume = RegisterVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterVolumeResponse'
            Prelude.<$> (x Prelude..?> "VolumeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterVolume

instance Prelude.NFData RegisterVolume

instance Prelude.ToHeaders RegisterVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.RegisterVolume" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterVolume where
  toJSON RegisterVolume' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Ec2VolumeId" Prelude..=) Prelude.<$> ec2VolumeId,
            Prelude.Just ("StackId" Prelude..= stackId)
          ]
      )

instance Prelude.ToPath RegisterVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterVolume where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @RegisterVolume@ request.
--
-- /See:/ 'newRegisterVolumeResponse' smart constructor.
data RegisterVolumeResponse = RegisterVolumeResponse'
  { -- | The volume ID.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  RegisterVolumeResponse
newRegisterVolumeResponse pHttpStatus_ =
  RegisterVolumeResponse'
    { volumeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The volume ID.
registerVolumeResponse_volumeId :: Lens.Lens' RegisterVolumeResponse (Prelude.Maybe Prelude.Text)
registerVolumeResponse_volumeId = Lens.lens (\RegisterVolumeResponse' {volumeId} -> volumeId) (\s@RegisterVolumeResponse' {} a -> s {volumeId = a} :: RegisterVolumeResponse)

-- | The response's http status code.
registerVolumeResponse_httpStatus :: Lens.Lens' RegisterVolumeResponse Prelude.Int
registerVolumeResponse_httpStatus = Lens.lens (\RegisterVolumeResponse' {httpStatus} -> httpStatus) (\s@RegisterVolumeResponse' {} a -> s {httpStatus = a} :: RegisterVolumeResponse)

instance Prelude.NFData RegisterVolumeResponse
