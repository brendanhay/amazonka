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
-- Module      : Amazonka.MediaConnect.DeregisterGatewayInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an instance. Before you deregister an instance, all bridges
-- running on the instance must be stopped. If you want to deregister an
-- instance without stopping the bridges, you must use the --force option.
module Amazonka.MediaConnect.DeregisterGatewayInstance
  ( -- * Creating a Request
    DeregisterGatewayInstance (..),
    newDeregisterGatewayInstance,

    -- * Request Lenses
    deregisterGatewayInstance_force,
    deregisterGatewayInstance_gatewayInstanceArn,

    -- * Destructuring the Response
    DeregisterGatewayInstanceResponse (..),
    newDeregisterGatewayInstanceResponse,

    -- * Response Lenses
    deregisterGatewayInstanceResponse_gatewayInstanceArn,
    deregisterGatewayInstanceResponse_instanceState,
    deregisterGatewayInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterGatewayInstance' smart constructor.
data DeregisterGatewayInstance = DeregisterGatewayInstance'
  { -- | Force the deregistration of an instance. Force will deregister an
    -- instance, even if there are bridges running on it.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the gateway that contains the instance
    -- that you want to deregister.
    gatewayInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterGatewayInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deregisterGatewayInstance_force' - Force the deregistration of an instance. Force will deregister an
-- instance, even if there are bridges running on it.
--
-- 'gatewayInstanceArn', 'deregisterGatewayInstance_gatewayInstanceArn' - The Amazon Resource Name (ARN) of the gateway that contains the instance
-- that you want to deregister.
newDeregisterGatewayInstance ::
  -- | 'gatewayInstanceArn'
  Prelude.Text ->
  DeregisterGatewayInstance
newDeregisterGatewayInstance pGatewayInstanceArn_ =
  DeregisterGatewayInstance'
    { force = Prelude.Nothing,
      gatewayInstanceArn = pGatewayInstanceArn_
    }

-- | Force the deregistration of an instance. Force will deregister an
-- instance, even if there are bridges running on it.
deregisterGatewayInstance_force :: Lens.Lens' DeregisterGatewayInstance (Prelude.Maybe Prelude.Bool)
deregisterGatewayInstance_force = Lens.lens (\DeregisterGatewayInstance' {force} -> force) (\s@DeregisterGatewayInstance' {} a -> s {force = a} :: DeregisterGatewayInstance)

-- | The Amazon Resource Name (ARN) of the gateway that contains the instance
-- that you want to deregister.
deregisterGatewayInstance_gatewayInstanceArn :: Lens.Lens' DeregisterGatewayInstance Prelude.Text
deregisterGatewayInstance_gatewayInstanceArn = Lens.lens (\DeregisterGatewayInstance' {gatewayInstanceArn} -> gatewayInstanceArn) (\s@DeregisterGatewayInstance' {} a -> s {gatewayInstanceArn = a} :: DeregisterGatewayInstance)

instance Core.AWSRequest DeregisterGatewayInstance where
  type
    AWSResponse DeregisterGatewayInstance =
      DeregisterGatewayInstanceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterGatewayInstanceResponse'
            Prelude.<$> (x Data..?> "gatewayInstanceArn")
            Prelude.<*> (x Data..?> "instanceState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterGatewayInstance where
  hashWithSalt _salt DeregisterGatewayInstance' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` gatewayInstanceArn

instance Prelude.NFData DeregisterGatewayInstance where
  rnf DeregisterGatewayInstance' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf gatewayInstanceArn

instance Data.ToHeaders DeregisterGatewayInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeregisterGatewayInstance where
  toPath DeregisterGatewayInstance' {..} =
    Prelude.mconcat
      [ "/v1/gateway-instances/",
        Data.toBS gatewayInstanceArn
      ]

instance Data.ToQuery DeregisterGatewayInstance where
  toQuery DeregisterGatewayInstance' {..} =
    Prelude.mconcat ["force" Data.=: force]

-- | /See:/ 'newDeregisterGatewayInstanceResponse' smart constructor.
data DeregisterGatewayInstanceResponse = DeregisterGatewayInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the instance.
    gatewayInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the instance.
    instanceState :: Prelude.Maybe InstanceState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterGatewayInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayInstanceArn', 'deregisterGatewayInstanceResponse_gatewayInstanceArn' - The Amazon Resource Name (ARN) of the instance.
--
-- 'instanceState', 'deregisterGatewayInstanceResponse_instanceState' - The status of the instance.
--
-- 'httpStatus', 'deregisterGatewayInstanceResponse_httpStatus' - The response's http status code.
newDeregisterGatewayInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterGatewayInstanceResponse
newDeregisterGatewayInstanceResponse pHttpStatus_ =
  DeregisterGatewayInstanceResponse'
    { gatewayInstanceArn =
        Prelude.Nothing,
      instanceState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the instance.
deregisterGatewayInstanceResponse_gatewayInstanceArn :: Lens.Lens' DeregisterGatewayInstanceResponse (Prelude.Maybe Prelude.Text)
deregisterGatewayInstanceResponse_gatewayInstanceArn = Lens.lens (\DeregisterGatewayInstanceResponse' {gatewayInstanceArn} -> gatewayInstanceArn) (\s@DeregisterGatewayInstanceResponse' {} a -> s {gatewayInstanceArn = a} :: DeregisterGatewayInstanceResponse)

-- | The status of the instance.
deregisterGatewayInstanceResponse_instanceState :: Lens.Lens' DeregisterGatewayInstanceResponse (Prelude.Maybe InstanceState)
deregisterGatewayInstanceResponse_instanceState = Lens.lens (\DeregisterGatewayInstanceResponse' {instanceState} -> instanceState) (\s@DeregisterGatewayInstanceResponse' {} a -> s {instanceState = a} :: DeregisterGatewayInstanceResponse)

-- | The response's http status code.
deregisterGatewayInstanceResponse_httpStatus :: Lens.Lens' DeregisterGatewayInstanceResponse Prelude.Int
deregisterGatewayInstanceResponse_httpStatus = Lens.lens (\DeregisterGatewayInstanceResponse' {httpStatus} -> httpStatus) (\s@DeregisterGatewayInstanceResponse' {} a -> s {httpStatus = a} :: DeregisterGatewayInstanceResponse)

instance
  Prelude.NFData
    DeregisterGatewayInstanceResponse
  where
  rnf DeregisterGatewayInstanceResponse' {..} =
    Prelude.rnf gatewayInstanceArn
      `Prelude.seq` Prelude.rnf instanceState
      `Prelude.seq` Prelude.rnf httpStatus
