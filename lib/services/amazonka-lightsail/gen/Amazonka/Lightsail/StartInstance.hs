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
-- Module      : Amazonka.Lightsail.StartInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific Amazon Lightsail instance from a stopped state. To
-- restart an instance, use the @reboot instance@ operation.
--
-- When you start a stopped instance, Lightsail assigns a new public IP
-- address to the instance. To use the same IP address after stopping and
-- starting an instance, create a static IP address and attach it to the
-- instance. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/lightsail-create-static-ip Amazon Lightsail Developer Guide>.
--
-- The @start instance@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @instance name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.StartInstance
  ( -- * Creating a Request
    StartInstance (..),
    newStartInstance,

    -- * Request Lenses
    startInstance_instanceName,

    -- * Destructuring the Response
    StartInstanceResponse (..),
    newStartInstanceResponse,

    -- * Response Lenses
    startInstanceResponse_operations,
    startInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartInstance' smart constructor.
data StartInstance = StartInstance'
  { -- | The name of the instance (a virtual private server) to start.
    instanceName :: Prelude.Text
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
-- 'instanceName', 'startInstance_instanceName' - The name of the instance (a virtual private server) to start.
newStartInstance ::
  -- | 'instanceName'
  Prelude.Text ->
  StartInstance
newStartInstance pInstanceName_ =
  StartInstance' {instanceName = pInstanceName_}

-- | The name of the instance (a virtual private server) to start.
startInstance_instanceName :: Lens.Lens' StartInstance Prelude.Text
startInstance_instanceName = Lens.lens (\StartInstance' {instanceName} -> instanceName) (\s@StartInstance' {} a -> s {instanceName = a} :: StartInstance)

instance Core.AWSRequest StartInstance where
  type
    AWSResponse StartInstance =
      StartInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartInstanceResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartInstance where
  hashWithSalt _salt StartInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceName

instance Prelude.NFData StartInstance where
  rnf StartInstance' {..} = Prelude.rnf instanceName

instance Data.ToHeaders StartInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.StartInstance" ::
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
          [Prelude.Just ("instanceName" Data..= instanceName)]
      )

instance Data.ToPath StartInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery StartInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'startInstanceResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'startInstanceResponse_httpStatus' - The response's http status code.
newStartInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartInstanceResponse
newStartInstanceResponse pHttpStatus_ =
  StartInstanceResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
startInstanceResponse_operations :: Lens.Lens' StartInstanceResponse (Prelude.Maybe [Operation])
startInstanceResponse_operations = Lens.lens (\StartInstanceResponse' {operations} -> operations) (\s@StartInstanceResponse' {} a -> s {operations = a} :: StartInstanceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startInstanceResponse_httpStatus :: Lens.Lens' StartInstanceResponse Prelude.Int
startInstanceResponse_httpStatus = Lens.lens (\StartInstanceResponse' {httpStatus} -> httpStatus) (\s@StartInstanceResponse' {} a -> s {httpStatus = a} :: StartInstanceResponse)

instance Prelude.NFData StartInstanceResponse where
  rnf StartInstanceResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
