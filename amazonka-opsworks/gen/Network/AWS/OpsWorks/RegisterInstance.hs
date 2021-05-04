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
-- Module      : Network.AWS.OpsWorks.RegisterInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.RegisterInstance
  ( -- * Creating a Request
    RegisterInstance (..),
    newRegisterInstance,

    -- * Request Lenses
    registerInstance_hostname,
    registerInstance_rsaPublicKey,
    registerInstance_instanceIdentity,
    registerInstance_privateIp,
    registerInstance_rsaPublicKeyFingerprint,
    registerInstance_publicIp,
    registerInstance_stackId,

    -- * Destructuring the Response
    RegisterInstanceResponse (..),
    newRegisterInstanceResponse,

    -- * Response Lenses
    registerInstanceResponse_instanceId,
    registerInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterInstance' smart constructor.
data RegisterInstance = RegisterInstance'
  { -- | The instance\'s hostname.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The instances public RSA key. This key is used to encrypt communication
    -- between the instance and the service.
    rsaPublicKey :: Prelude.Maybe Prelude.Text,
    -- | An InstanceIdentity object that contains the instance\'s identity.
    instanceIdentity :: Prelude.Maybe InstanceIdentity,
    -- | The instance\'s private IP address.
    privateIp :: Prelude.Maybe Prelude.Text,
    -- | The instances public RSA key fingerprint.
    rsaPublicKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s public IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack that the instance is to be registered with.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'registerInstance_hostname' - The instance\'s hostname.
--
-- 'rsaPublicKey', 'registerInstance_rsaPublicKey' - The instances public RSA key. This key is used to encrypt communication
-- between the instance and the service.
--
-- 'instanceIdentity', 'registerInstance_instanceIdentity' - An InstanceIdentity object that contains the instance\'s identity.
--
-- 'privateIp', 'registerInstance_privateIp' - The instance\'s private IP address.
--
-- 'rsaPublicKeyFingerprint', 'registerInstance_rsaPublicKeyFingerprint' - The instances public RSA key fingerprint.
--
-- 'publicIp', 'registerInstance_publicIp' - The instance\'s public IP address.
--
-- 'stackId', 'registerInstance_stackId' - The ID of the stack that the instance is to be registered with.
newRegisterInstance ::
  -- | 'stackId'
  Prelude.Text ->
  RegisterInstance
newRegisterInstance pStackId_ =
  RegisterInstance'
    { hostname = Prelude.Nothing,
      rsaPublicKey = Prelude.Nothing,
      instanceIdentity = Prelude.Nothing,
      privateIp = Prelude.Nothing,
      rsaPublicKeyFingerprint = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      stackId = pStackId_
    }

-- | The instance\'s hostname.
registerInstance_hostname :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_hostname = Lens.lens (\RegisterInstance' {hostname} -> hostname) (\s@RegisterInstance' {} a -> s {hostname = a} :: RegisterInstance)

-- | The instances public RSA key. This key is used to encrypt communication
-- between the instance and the service.
registerInstance_rsaPublicKey :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_rsaPublicKey = Lens.lens (\RegisterInstance' {rsaPublicKey} -> rsaPublicKey) (\s@RegisterInstance' {} a -> s {rsaPublicKey = a} :: RegisterInstance)

-- | An InstanceIdentity object that contains the instance\'s identity.
registerInstance_instanceIdentity :: Lens.Lens' RegisterInstance (Prelude.Maybe InstanceIdentity)
registerInstance_instanceIdentity = Lens.lens (\RegisterInstance' {instanceIdentity} -> instanceIdentity) (\s@RegisterInstance' {} a -> s {instanceIdentity = a} :: RegisterInstance)

-- | The instance\'s private IP address.
registerInstance_privateIp :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_privateIp = Lens.lens (\RegisterInstance' {privateIp} -> privateIp) (\s@RegisterInstance' {} a -> s {privateIp = a} :: RegisterInstance)

-- | The instances public RSA key fingerprint.
registerInstance_rsaPublicKeyFingerprint :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_rsaPublicKeyFingerprint = Lens.lens (\RegisterInstance' {rsaPublicKeyFingerprint} -> rsaPublicKeyFingerprint) (\s@RegisterInstance' {} a -> s {rsaPublicKeyFingerprint = a} :: RegisterInstance)

-- | The instance\'s public IP address.
registerInstance_publicIp :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_publicIp = Lens.lens (\RegisterInstance' {publicIp} -> publicIp) (\s@RegisterInstance' {} a -> s {publicIp = a} :: RegisterInstance)

-- | The ID of the stack that the instance is to be registered with.
registerInstance_stackId :: Lens.Lens' RegisterInstance Prelude.Text
registerInstance_stackId = Lens.lens (\RegisterInstance' {stackId} -> stackId) (\s@RegisterInstance' {} a -> s {stackId = a} :: RegisterInstance)

instance Prelude.AWSRequest RegisterInstance where
  type Rs RegisterInstance = RegisterInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterInstanceResponse'
            Prelude.<$> (x Prelude..?> "InstanceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterInstance

instance Prelude.NFData RegisterInstance

instance Prelude.ToHeaders RegisterInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.RegisterInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterInstance where
  toJSON RegisterInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Hostname" Prelude..=) Prelude.<$> hostname,
            ("RsaPublicKey" Prelude..=) Prelude.<$> rsaPublicKey,
            ("InstanceIdentity" Prelude..=)
              Prelude.<$> instanceIdentity,
            ("PrivateIp" Prelude..=) Prelude.<$> privateIp,
            ("RsaPublicKeyFingerprint" Prelude..=)
              Prelude.<$> rsaPublicKeyFingerprint,
            ("PublicIp" Prelude..=) Prelude.<$> publicIp,
            Prelude.Just ("StackId" Prelude..= stackId)
          ]
      )

instance Prelude.ToPath RegisterInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterInstance where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData RegisterInstanceResponse
