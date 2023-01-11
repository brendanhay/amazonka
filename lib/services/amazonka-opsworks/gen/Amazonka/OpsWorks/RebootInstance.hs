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
-- Module      : Amazonka.OpsWorks.RebootInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.OpsWorks.RebootInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRebootInstance' smart constructor.
data RebootInstance = RebootInstance'
  { -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest RebootInstance where
  type
    AWSResponse RebootInstance =
      RebootInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RebootInstanceResponse'

instance Prelude.Hashable RebootInstance where
  hashWithSalt _salt RebootInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceId

instance Prelude.NFData RebootInstance where
  rnf RebootInstance' {..} = Prelude.rnf instanceId

instance Data.ToHeaders RebootInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.RebootInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RebootInstance where
  toJSON RebootInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceId" Data..= instanceId)]
      )

instance Data.ToPath RebootInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery RebootInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRebootInstanceResponse ::
  RebootInstanceResponse
newRebootInstanceResponse = RebootInstanceResponse'

instance Prelude.NFData RebootInstanceResponse where
  rnf _ = ()
