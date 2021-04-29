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
-- Module      : Network.AWS.OpsWorks.RebootInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a specified instance. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.RebootInstance
  ( -- * Creating a Request
    RebootInstance (..),
    newRebootInstance,

    -- * Request Lenses
    rebootInstance_instanceId,

    -- * Destructuring the Response
    RebootInstanceResponse (..),
    newRebootInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebootInstance' smart constructor.
data RebootInstance = RebootInstance'
  { -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebootInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'rebootInstance_instanceId' - The instance ID.
newRebootInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  RebootInstance
newRebootInstance pInstanceId_ =
  RebootInstance' {instanceId = pInstanceId_}

-- | The instance ID.
rebootInstance_instanceId :: Lens.Lens' RebootInstance Prelude.Text
rebootInstance_instanceId = Lens.lens (\RebootInstance' {instanceId} -> instanceId) (\s@RebootInstance' {} a -> s {instanceId = a} :: RebootInstance)

instance Prelude.AWSRequest RebootInstance where
  type Rs RebootInstance = RebootInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RebootInstanceResponse'

instance Prelude.Hashable RebootInstance

instance Prelude.NFData RebootInstance

instance Prelude.ToHeaders RebootInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.RebootInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RebootInstance where
  toJSON RebootInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceId" Prelude..= instanceId)]
      )

instance Prelude.ToPath RebootInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RebootInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebootInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRebootInstanceResponse ::
  RebootInstanceResponse
newRebootInstanceResponse = RebootInstanceResponse'

instance Prelude.NFData RebootInstanceResponse
