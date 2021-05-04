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
-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specified instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.StartInstance
  ( -- * Creating a Request
    StartInstance (..),
    newStartInstance,

    -- * Request Lenses
    startInstance_instanceId,

    -- * Destructuring the Response
    StartInstanceResponse (..),
    newStartInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartInstance' smart constructor.
data StartInstance = StartInstance'
  { -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'startInstance_instanceId' - The instance ID.
newStartInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  StartInstance
newStartInstance pInstanceId_ =
  StartInstance' {instanceId = pInstanceId_}

-- | The instance ID.
startInstance_instanceId :: Lens.Lens' StartInstance Prelude.Text
startInstance_instanceId = Lens.lens (\StartInstance' {instanceId} -> instanceId) (\s@StartInstance' {} a -> s {instanceId = a} :: StartInstance)

instance Prelude.AWSRequest StartInstance where
  type Rs StartInstance = StartInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StartInstanceResponse'

instance Prelude.Hashable StartInstance

instance Prelude.NFData StartInstance

instance Prelude.ToHeaders StartInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.StartInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartInstance where
  toJSON StartInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceId" Prelude..= instanceId)]
      )

instance Prelude.ToPath StartInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartInstanceResponse ::
  StartInstanceResponse
newStartInstanceResponse = StartInstanceResponse'

instance Prelude.NFData StartInstanceResponse
