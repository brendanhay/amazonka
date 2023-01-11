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
-- Module      : Amazonka.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.OpsWorks.StartInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartInstance' smart constructor.
data StartInstance = StartInstance'
  { -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest StartInstance where
  type
    AWSResponse StartInstance =
      StartInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StartInstanceResponse'

instance Prelude.Hashable StartInstance where
  hashWithSalt _salt StartInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceId

instance Prelude.NFData StartInstance where
  rnf StartInstance' {..} = Prelude.rnf instanceId

instance Data.ToHeaders StartInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.StartInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartInstance where
  toJSON StartInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceId" Data..= instanceId)]
      )

instance Data.ToPath StartInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery StartInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartInstanceResponse ::
  StartInstanceResponse
newStartInstanceResponse = StartInstanceResponse'

instance Prelude.NFData StartInstanceResponse where
  rnf _ = ()
