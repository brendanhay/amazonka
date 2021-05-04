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
-- Module      : Network.AWS.OpsWorks.DeregisterInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregister a registered Amazon EC2 or on-premises instance. This action
-- removes the instance from the stack and returns it to your control. This
-- action cannot be used with instances that were created with AWS OpsWorks
-- Stacks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DeregisterInstance
  ( -- * Creating a Request
    DeregisterInstance (..),
    newDeregisterInstance,

    -- * Request Lenses
    deregisterInstance_instanceId,

    -- * Destructuring the Response
    DeregisterInstanceResponse (..),
    newDeregisterInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterInstance' smart constructor.
data DeregisterInstance = DeregisterInstance'
  { -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deregisterInstance_instanceId' - The instance ID.
newDeregisterInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  DeregisterInstance
newDeregisterInstance pInstanceId_ =
  DeregisterInstance' {instanceId = pInstanceId_}

-- | The instance ID.
deregisterInstance_instanceId :: Lens.Lens' DeregisterInstance Prelude.Text
deregisterInstance_instanceId = Lens.lens (\DeregisterInstance' {instanceId} -> instanceId) (\s@DeregisterInstance' {} a -> s {instanceId = a} :: DeregisterInstance)

instance Prelude.AWSRequest DeregisterInstance where
  type
    Rs DeregisterInstance =
      DeregisterInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeregisterInstanceResponse'

instance Prelude.Hashable DeregisterInstance

instance Prelude.NFData DeregisterInstance

instance Prelude.ToHeaders DeregisterInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DeregisterInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterInstance where
  toJSON DeregisterInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceId" Prelude..= instanceId)]
      )

instance Prelude.ToPath DeregisterInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterInstanceResponse ::
  DeregisterInstanceResponse
newDeregisterInstanceResponse =
  DeregisterInstanceResponse'

instance Prelude.NFData DeregisterInstanceResponse
