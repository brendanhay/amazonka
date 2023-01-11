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
-- Module      : Amazonka.Lightsail.CloseInstancePublicPorts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes ports for a specific Amazon Lightsail instance.
--
-- The @CloseInstancePublicPorts@ action supports tag-based access control
-- via resource tags applied to the resource identified by @instanceName@.
-- For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CloseInstancePublicPorts
  ( -- * Creating a Request
    CloseInstancePublicPorts (..),
    newCloseInstancePublicPorts,

    -- * Request Lenses
    closeInstancePublicPorts_portInfo,
    closeInstancePublicPorts_instanceName,

    -- * Destructuring the Response
    CloseInstancePublicPortsResponse (..),
    newCloseInstancePublicPortsResponse,

    -- * Response Lenses
    closeInstancePublicPortsResponse_operation,
    closeInstancePublicPortsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCloseInstancePublicPorts' smart constructor.
data CloseInstancePublicPorts = CloseInstancePublicPorts'
  { -- | An object to describe the ports to close for the specified instance.
    portInfo :: PortInfo,
    -- | The name of the instance for which to close ports.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloseInstancePublicPorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portInfo', 'closeInstancePublicPorts_portInfo' - An object to describe the ports to close for the specified instance.
--
-- 'instanceName', 'closeInstancePublicPorts_instanceName' - The name of the instance for which to close ports.
newCloseInstancePublicPorts ::
  -- | 'portInfo'
  PortInfo ->
  -- | 'instanceName'
  Prelude.Text ->
  CloseInstancePublicPorts
newCloseInstancePublicPorts pPortInfo_ pInstanceName_ =
  CloseInstancePublicPorts'
    { portInfo = pPortInfo_,
      instanceName = pInstanceName_
    }

-- | An object to describe the ports to close for the specified instance.
closeInstancePublicPorts_portInfo :: Lens.Lens' CloseInstancePublicPorts PortInfo
closeInstancePublicPorts_portInfo = Lens.lens (\CloseInstancePublicPorts' {portInfo} -> portInfo) (\s@CloseInstancePublicPorts' {} a -> s {portInfo = a} :: CloseInstancePublicPorts)

-- | The name of the instance for which to close ports.
closeInstancePublicPorts_instanceName :: Lens.Lens' CloseInstancePublicPorts Prelude.Text
closeInstancePublicPorts_instanceName = Lens.lens (\CloseInstancePublicPorts' {instanceName} -> instanceName) (\s@CloseInstancePublicPorts' {} a -> s {instanceName = a} :: CloseInstancePublicPorts)

instance Core.AWSRequest CloseInstancePublicPorts where
  type
    AWSResponse CloseInstancePublicPorts =
      CloseInstancePublicPortsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CloseInstancePublicPortsResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CloseInstancePublicPorts where
  hashWithSalt _salt CloseInstancePublicPorts' {..} =
    _salt `Prelude.hashWithSalt` portInfo
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData CloseInstancePublicPorts where
  rnf CloseInstancePublicPorts' {..} =
    Prelude.rnf portInfo
      `Prelude.seq` Prelude.rnf instanceName

instance Data.ToHeaders CloseInstancePublicPorts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CloseInstancePublicPorts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CloseInstancePublicPorts where
  toJSON CloseInstancePublicPorts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("portInfo" Data..= portInfo),
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath CloseInstancePublicPorts where
  toPath = Prelude.const "/"

instance Data.ToQuery CloseInstancePublicPorts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCloseInstancePublicPortsResponse' smart constructor.
data CloseInstancePublicPortsResponse = CloseInstancePublicPortsResponse'
  { -- | An object that describes the result of the action, such as the status of
    -- the request, the timestamp of the request, and the resources affected by
    -- the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloseInstancePublicPortsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'closeInstancePublicPortsResponse_operation' - An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
--
-- 'httpStatus', 'closeInstancePublicPortsResponse_httpStatus' - The response's http status code.
newCloseInstancePublicPortsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CloseInstancePublicPortsResponse
newCloseInstancePublicPortsResponse pHttpStatus_ =
  CloseInstancePublicPortsResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
closeInstancePublicPortsResponse_operation :: Lens.Lens' CloseInstancePublicPortsResponse (Prelude.Maybe Operation)
closeInstancePublicPortsResponse_operation = Lens.lens (\CloseInstancePublicPortsResponse' {operation} -> operation) (\s@CloseInstancePublicPortsResponse' {} a -> s {operation = a} :: CloseInstancePublicPortsResponse)

-- | The response's http status code.
closeInstancePublicPortsResponse_httpStatus :: Lens.Lens' CloseInstancePublicPortsResponse Prelude.Int
closeInstancePublicPortsResponse_httpStatus = Lens.lens (\CloseInstancePublicPortsResponse' {httpStatus} -> httpStatus) (\s@CloseInstancePublicPortsResponse' {} a -> s {httpStatus = a} :: CloseInstancePublicPortsResponse)

instance
  Prelude.NFData
    CloseInstancePublicPortsResponse
  where
  rnf CloseInstancePublicPortsResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
