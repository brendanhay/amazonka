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
-- Module      : Amazonka.OpsWorks.StopInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified instance. When you stop a standard instance, the data
-- disappears and must be reinstalled when you restart the instance. You
-- can stop an Amazon EBS-backed instance without losing data. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.StopInstance
  ( -- * Creating a Request
    StopInstance (..),
    newStopInstance,

    -- * Request Lenses
    stopInstance_force,
    stopInstance_instanceId,

    -- * Destructuring the Response
    StopInstanceResponse (..),
    newStopInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopInstance' smart constructor.
data StopInstance = StopInstance'
  { -- | Specifies whether to force an instance to stop. If the instance\'s root
    -- device type is @ebs@, or EBS-backed, adding the @Force@ parameter to the
    -- @StopInstances@ API call disassociates the AWS OpsWorks Stacks instance
    -- from EC2, and forces deletion of /only/ the OpsWorks Stacks instance.
    -- You must also delete the formerly-associated instance in EC2 after
    -- troubleshooting and replacing the AWS OpsWorks Stacks instance with a
    -- new one.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'stopInstance_force' - Specifies whether to force an instance to stop. If the instance\'s root
-- device type is @ebs@, or EBS-backed, adding the @Force@ parameter to the
-- @StopInstances@ API call disassociates the AWS OpsWorks Stacks instance
-- from EC2, and forces deletion of /only/ the OpsWorks Stacks instance.
-- You must also delete the formerly-associated instance in EC2 after
-- troubleshooting and replacing the AWS OpsWorks Stacks instance with a
-- new one.
--
-- 'instanceId', 'stopInstance_instanceId' - The instance ID.
newStopInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  StopInstance
newStopInstance pInstanceId_ =
  StopInstance'
    { force = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Specifies whether to force an instance to stop. If the instance\'s root
-- device type is @ebs@, or EBS-backed, adding the @Force@ parameter to the
-- @StopInstances@ API call disassociates the AWS OpsWorks Stacks instance
-- from EC2, and forces deletion of /only/ the OpsWorks Stacks instance.
-- You must also delete the formerly-associated instance in EC2 after
-- troubleshooting and replacing the AWS OpsWorks Stacks instance with a
-- new one.
stopInstance_force :: Lens.Lens' StopInstance (Prelude.Maybe Prelude.Bool)
stopInstance_force = Lens.lens (\StopInstance' {force} -> force) (\s@StopInstance' {} a -> s {force = a} :: StopInstance)

-- | The instance ID.
stopInstance_instanceId :: Lens.Lens' StopInstance Prelude.Text
stopInstance_instanceId = Lens.lens (\StopInstance' {instanceId} -> instanceId) (\s@StopInstance' {} a -> s {instanceId = a} :: StopInstance)

instance Core.AWSRequest StopInstance where
  type AWSResponse StopInstance = StopInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull StopInstanceResponse'

instance Prelude.Hashable StopInstance where
  hashWithSalt _salt StopInstance' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData StopInstance where
  rnf StopInstance' {..} =
    Prelude.rnf force `Prelude.seq`
      Prelude.rnf instanceId

instance Data.ToHeaders StopInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.StopInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopInstance where
  toJSON StopInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Force" Data..=) Prelude.<$> force,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath StopInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery StopInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopInstanceResponse ::
  StopInstanceResponse
newStopInstanceResponse = StopInstanceResponse'

instance Prelude.NFData StopInstanceResponse where
  rnf _ = ()
