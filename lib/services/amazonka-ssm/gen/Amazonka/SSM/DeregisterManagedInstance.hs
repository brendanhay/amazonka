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
-- Module      : Amazonka.SSM.DeregisterManagedInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the server or virtual machine from the list of registered
-- servers. You can reregister the node again at any time. If you don\'t
-- plan to use Run Command on the server, we suggest uninstalling SSM Agent
-- first.
module Amazonka.SSM.DeregisterManagedInstance
  ( -- * Creating a Request
    DeregisterManagedInstance (..),
    newDeregisterManagedInstance,

    -- * Request Lenses
    deregisterManagedInstance_instanceId,

    -- * Destructuring the Response
    DeregisterManagedInstanceResponse (..),
    newDeregisterManagedInstanceResponse,

    -- * Response Lenses
    deregisterManagedInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeregisterManagedInstance' smart constructor.
data DeregisterManagedInstance = DeregisterManagedInstance'
  { -- | The ID assigned to the managed node when you registered it using the
    -- activation process.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterManagedInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deregisterManagedInstance_instanceId' - The ID assigned to the managed node when you registered it using the
-- activation process.
newDeregisterManagedInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  DeregisterManagedInstance
newDeregisterManagedInstance pInstanceId_ =
  DeregisterManagedInstance'
    { instanceId =
        pInstanceId_
    }

-- | The ID assigned to the managed node when you registered it using the
-- activation process.
deregisterManagedInstance_instanceId :: Lens.Lens' DeregisterManagedInstance Prelude.Text
deregisterManagedInstance_instanceId = Lens.lens (\DeregisterManagedInstance' {instanceId} -> instanceId) (\s@DeregisterManagedInstance' {} a -> s {instanceId = a} :: DeregisterManagedInstance)

instance Core.AWSRequest DeregisterManagedInstance where
  type
    AWSResponse DeregisterManagedInstance =
      DeregisterManagedInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterManagedInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterManagedInstance where
  hashWithSalt _salt DeregisterManagedInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DeregisterManagedInstance where
  rnf DeregisterManagedInstance' {..} =
    Prelude.rnf instanceId

instance Core.ToHeaders DeregisterManagedInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DeregisterManagedInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeregisterManagedInstance where
  toJSON DeregisterManagedInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceId" Core..= instanceId)]
      )

instance Core.ToPath DeregisterManagedInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery DeregisterManagedInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterManagedInstanceResponse' smart constructor.
data DeregisterManagedInstanceResponse = DeregisterManagedInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterManagedInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterManagedInstanceResponse_httpStatus' - The response's http status code.
newDeregisterManagedInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterManagedInstanceResponse
newDeregisterManagedInstanceResponse pHttpStatus_ =
  DeregisterManagedInstanceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterManagedInstanceResponse_httpStatus :: Lens.Lens' DeregisterManagedInstanceResponse Prelude.Int
deregisterManagedInstanceResponse_httpStatus = Lens.lens (\DeregisterManagedInstanceResponse' {httpStatus} -> httpStatus) (\s@DeregisterManagedInstanceResponse' {} a -> s {httpStatus = a} :: DeregisterManagedInstanceResponse)

instance
  Prelude.NFData
    DeregisterManagedInstanceResponse
  where
  rnf DeregisterManagedInstanceResponse' {..} =
    Prelude.rnf httpStatus
