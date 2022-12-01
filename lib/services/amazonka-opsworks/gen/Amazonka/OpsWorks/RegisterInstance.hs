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
-- Module      : Amazonka.OpsWorks.RegisterInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers instances that were created outside of AWS OpsWorks Stacks
-- with a specified stack.
--
-- We do not recommend using this action to register instances. The
-- complete registration operation includes two tasks: installing the AWS
-- OpsWorks Stacks agent on the instance, and registering the instance with
-- the stack. @RegisterInstance@ handles only the second step. You should
-- instead use the AWS CLI @register@ command, which performs the entire
-- registration operation. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register.html Registering an Instance with an AWS OpsWorks Stacks Stack>.
--
-- Registered instances have the same requirements as instances that are
-- created by using the CreateInstance API. For example, registered
-- instances must be running a supported Linux-based operating system, and
-- they must have a supported instance type. For more information about
-- requirements for instances that you want to register, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register-registering-preparer.html Preparing the Instance>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.RegisterInstance
  ( -- * Creating a Request
    RegisterInstance (..),
    newRegisterInstance,

    -- * Request Lenses
    registerInstance_rsaPublicKeyFingerprint,
    registerInstance_hostname,
    registerInstance_instanceIdentity,
    registerInstance_publicIp,
    registerInstance_rsaPublicKey,
    registerInstance_privateIp,
    registerInstance_stackId,

    -- * Destructuring the Response
    RegisterInstanceResponse (..),
    newRegisterInstanceResponse,

    -- * Response Lenses
    registerInstanceResponse_instanceId,
    registerInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterInstance' smart constructor.
data RegisterInstance = RegisterInstance'
  { -- | The instances public RSA key fingerprint.
    rsaPublicKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s hostname.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | An InstanceIdentity object that contains the instance\'s identity.
    instanceIdentity :: Prelude.Maybe InstanceIdentity,
    -- | The instance\'s public IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The instances public RSA key. This key is used to encrypt communication
    -- between the instance and the service.
    rsaPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s private IP address.
    privateIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack that the instance is to be registered with.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rsaPublicKeyFingerprint', 'registerInstance_rsaPublicKeyFingerprint' - The instances public RSA key fingerprint.
--
-- 'hostname', 'registerInstance_hostname' - The instance\'s hostname.
--
-- 'instanceIdentity', 'registerInstance_instanceIdentity' - An InstanceIdentity object that contains the instance\'s identity.
--
-- 'publicIp', 'registerInstance_publicIp' - The instance\'s public IP address.
--
-- 'rsaPublicKey', 'registerInstance_rsaPublicKey' - The instances public RSA key. This key is used to encrypt communication
-- between the instance and the service.
--
-- 'privateIp', 'registerInstance_privateIp' - The instance\'s private IP address.
--
-- 'stackId', 'registerInstance_stackId' - The ID of the stack that the instance is to be registered with.
newRegisterInstance ::
  -- | 'stackId'
  Prelude.Text ->
  RegisterInstance
newRegisterInstance pStackId_ =
  RegisterInstance'
    { rsaPublicKeyFingerprint =
        Prelude.Nothing,
      hostname = Prelude.Nothing,
      instanceIdentity = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      rsaPublicKey = Prelude.Nothing,
      privateIp = Prelude.Nothing,
      stackId = pStackId_
    }

-- | The instances public RSA key fingerprint.
registerInstance_rsaPublicKeyFingerprint :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_rsaPublicKeyFingerprint = Lens.lens (\RegisterInstance' {rsaPublicKeyFingerprint} -> rsaPublicKeyFingerprint) (\s@RegisterInstance' {} a -> s {rsaPublicKeyFingerprint = a} :: RegisterInstance)

-- | The instance\'s hostname.
registerInstance_hostname :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_hostname = Lens.lens (\RegisterInstance' {hostname} -> hostname) (\s@RegisterInstance' {} a -> s {hostname = a} :: RegisterInstance)

-- | An InstanceIdentity object that contains the instance\'s identity.
registerInstance_instanceIdentity :: Lens.Lens' RegisterInstance (Prelude.Maybe InstanceIdentity)
registerInstance_instanceIdentity = Lens.lens (\RegisterInstance' {instanceIdentity} -> instanceIdentity) (\s@RegisterInstance' {} a -> s {instanceIdentity = a} :: RegisterInstance)

-- | The instance\'s public IP address.
registerInstance_publicIp :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_publicIp = Lens.lens (\RegisterInstance' {publicIp} -> publicIp) (\s@RegisterInstance' {} a -> s {publicIp = a} :: RegisterInstance)

-- | The instances public RSA key. This key is used to encrypt communication
-- between the instance and the service.
registerInstance_rsaPublicKey :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_rsaPublicKey = Lens.lens (\RegisterInstance' {rsaPublicKey} -> rsaPublicKey) (\s@RegisterInstance' {} a -> s {rsaPublicKey = a} :: RegisterInstance)

-- | The instance\'s private IP address.
registerInstance_privateIp :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_privateIp = Lens.lens (\RegisterInstance' {privateIp} -> privateIp) (\s@RegisterInstance' {} a -> s {privateIp = a} :: RegisterInstance)

-- | The ID of the stack that the instance is to be registered with.
registerInstance_stackId :: Lens.Lens' RegisterInstance Prelude.Text
registerInstance_stackId = Lens.lens (\RegisterInstance' {stackId} -> stackId) (\s@RegisterInstance' {} a -> s {stackId = a} :: RegisterInstance)

instance Core.AWSRequest RegisterInstance where
  type
    AWSResponse RegisterInstance =
      RegisterInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterInstanceResponse'
            Prelude.<$> (x Core..?> "InstanceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterInstance where
  hashWithSalt _salt RegisterInstance' {..} =
    _salt
      `Prelude.hashWithSalt` rsaPublicKeyFingerprint
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` instanceIdentity
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` rsaPublicKey
      `Prelude.hashWithSalt` privateIp
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData RegisterInstance where
  rnf RegisterInstance' {..} =
    Prelude.rnf rsaPublicKeyFingerprint
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf instanceIdentity
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf rsaPublicKey
      `Prelude.seq` Prelude.rnf privateIp
      `Prelude.seq` Prelude.rnf stackId

instance Core.ToHeaders RegisterInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.RegisterInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterInstance where
  toJSON RegisterInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RsaPublicKeyFingerprint" Core..=)
              Prelude.<$> rsaPublicKeyFingerprint,
            ("Hostname" Core..=) Prelude.<$> hostname,
            ("InstanceIdentity" Core..=)
              Prelude.<$> instanceIdentity,
            ("PublicIp" Core..=) Prelude.<$> publicIp,
            ("RsaPublicKey" Core..=) Prelude.<$> rsaPublicKey,
            ("PrivateIp" Core..=) Prelude.<$> privateIp,
            Prelude.Just ("StackId" Core..= stackId)
          ]
      )

instance Core.ToPath RegisterInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterInstance where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @RegisterInstanceResult@ request.
--
-- /See:/ 'newRegisterInstanceResponse' smart constructor.
data RegisterInstanceResponse = RegisterInstanceResponse'
  { -- | The registered instance\'s AWS OpsWorks Stacks ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'registerInstanceResponse_instanceId' - The registered instance\'s AWS OpsWorks Stacks ID.
--
-- 'httpStatus', 'registerInstanceResponse_httpStatus' - The response's http status code.
newRegisterInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterInstanceResponse
newRegisterInstanceResponse pHttpStatus_ =
  RegisterInstanceResponse'
    { instanceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registered instance\'s AWS OpsWorks Stacks ID.
registerInstanceResponse_instanceId :: Lens.Lens' RegisterInstanceResponse (Prelude.Maybe Prelude.Text)
registerInstanceResponse_instanceId = Lens.lens (\RegisterInstanceResponse' {instanceId} -> instanceId) (\s@RegisterInstanceResponse' {} a -> s {instanceId = a} :: RegisterInstanceResponse)

-- | The response's http status code.
registerInstanceResponse_httpStatus :: Lens.Lens' RegisterInstanceResponse Prelude.Int
registerInstanceResponse_httpStatus = Lens.lens (\RegisterInstanceResponse' {httpStatus} -> httpStatus) (\s@RegisterInstanceResponse' {} a -> s {httpStatus = a} :: RegisterInstanceResponse)

instance Prelude.NFData RegisterInstanceResponse where
  rnf RegisterInstanceResponse' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf httpStatus
