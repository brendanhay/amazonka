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
-- Module      : Amazonka.Lightsail.RebootInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific instance.
--
-- The @reboot instance@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @instance name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.RebootInstance
  ( -- * Creating a Request
    RebootInstance (..),
    newRebootInstance,

    -- * Request Lenses
    rebootInstance_instanceName,

    -- * Destructuring the Response
    RebootInstanceResponse (..),
    newRebootInstanceResponse,

    -- * Response Lenses
    rebootInstanceResponse_operations,
    rebootInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRebootInstance' smart constructor.
data RebootInstance = RebootInstance'
  { -- | The name of the instance to reboot.
    instanceName :: Prelude.Text
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
-- 'instanceName', 'rebootInstance_instanceName' - The name of the instance to reboot.
newRebootInstance ::
  -- | 'instanceName'
  Prelude.Text ->
  RebootInstance
newRebootInstance pInstanceName_ =
  RebootInstance' {instanceName = pInstanceName_}

-- | The name of the instance to reboot.
rebootInstance_instanceName :: Lens.Lens' RebootInstance Prelude.Text
rebootInstance_instanceName = Lens.lens (\RebootInstance' {instanceName} -> instanceName) (\s@RebootInstance' {} a -> s {instanceName = a} :: RebootInstance)

instance Core.AWSRequest RebootInstance where
  type
    AWSResponse RebootInstance =
      RebootInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootInstanceResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootInstance where
  hashWithSalt _salt RebootInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceName

instance Prelude.NFData RebootInstance where
  rnf RebootInstance' {..} = Prelude.rnf instanceName

instance Data.ToHeaders RebootInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.RebootInstance" ::
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
          [Prelude.Just ("instanceName" Data..= instanceName)]
      )

instance Data.ToPath RebootInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery RebootInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'rebootInstanceResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'rebootInstanceResponse_httpStatus' - The response's http status code.
newRebootInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootInstanceResponse
newRebootInstanceResponse pHttpStatus_ =
  RebootInstanceResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
rebootInstanceResponse_operations :: Lens.Lens' RebootInstanceResponse (Prelude.Maybe [Operation])
rebootInstanceResponse_operations = Lens.lens (\RebootInstanceResponse' {operations} -> operations) (\s@RebootInstanceResponse' {} a -> s {operations = a} :: RebootInstanceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
rebootInstanceResponse_httpStatus :: Lens.Lens' RebootInstanceResponse Prelude.Int
rebootInstanceResponse_httpStatus = Lens.lens (\RebootInstanceResponse' {httpStatus} -> httpStatus) (\s@RebootInstanceResponse' {} a -> s {httpStatus = a} :: RebootInstanceResponse)

instance Prelude.NFData RebootInstanceResponse where
  rnf RebootInstanceResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
