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
-- Module      : Amazonka.OpsWorks.UnassignInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns a registered instance from all layers that are using the
-- instance. The instance remains in the stack as an unassigned instance,
-- and can be assigned to another layer as needed. You cannot use this
-- action with instances that were created with AWS OpsWorks Stacks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information about user
-- permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.UnassignInstance
  ( -- * Creating a Request
    UnassignInstance (..),
    newUnassignInstance,

    -- * Request Lenses
    unassignInstance_instanceId,

    -- * Destructuring the Response
    UnassignInstanceResponse (..),
    newUnassignInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnassignInstance' smart constructor.
data UnassignInstance = UnassignInstance'
  { -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'unassignInstance_instanceId' - The instance ID.
newUnassignInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  UnassignInstance
newUnassignInstance pInstanceId_ =
  UnassignInstance' {instanceId = pInstanceId_}

-- | The instance ID.
unassignInstance_instanceId :: Lens.Lens' UnassignInstance Prelude.Text
unassignInstance_instanceId = Lens.lens (\UnassignInstance' {instanceId} -> instanceId) (\s@UnassignInstance' {} a -> s {instanceId = a} :: UnassignInstance)

instance Core.AWSRequest UnassignInstance where
  type
    AWSResponse UnassignInstance =
      UnassignInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UnassignInstanceResponse'

instance Prelude.Hashable UnassignInstance where
  hashWithSalt _salt UnassignInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UnassignInstance where
  rnf UnassignInstance' {..} = Prelude.rnf instanceId

instance Data.ToHeaders UnassignInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.UnassignInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnassignInstance where
  toJSON UnassignInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceId" Data..= instanceId)]
      )

instance Data.ToPath UnassignInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery UnassignInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnassignInstanceResponse' smart constructor.
data UnassignInstanceResponse = UnassignInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnassignInstanceResponse ::
  UnassignInstanceResponse
newUnassignInstanceResponse =
  UnassignInstanceResponse'

instance Prelude.NFData UnassignInstanceResponse where
  rnf _ = ()
