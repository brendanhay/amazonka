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
-- Module      : Amazonka.Lightsail.PutInstancePublicPorts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Opens ports for a specific Amazon Lightsail instance, and specifies the
-- IP addresses allowed to connect to the instance through the ports, and
-- the protocol. This action also closes all currently open ports that are
-- not included in the request. Include all of the ports and the protocols
-- you want to open in your @PutInstancePublicPorts@request. Or use the
-- @OpenInstancePublicPorts@ action to open ports without closing currently
-- open ports.
--
-- The @PutInstancePublicPorts@ action supports tag-based access control
-- via resource tags applied to the resource identified by @instanceName@.
-- For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.PutInstancePublicPorts
  ( -- * Creating a Request
    PutInstancePublicPorts (..),
    newPutInstancePublicPorts,

    -- * Request Lenses
    putInstancePublicPorts_portInfos,
    putInstancePublicPorts_instanceName,

    -- * Destructuring the Response
    PutInstancePublicPortsResponse (..),
    newPutInstancePublicPortsResponse,

    -- * Response Lenses
    putInstancePublicPortsResponse_operation,
    putInstancePublicPortsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutInstancePublicPorts' smart constructor.
data PutInstancePublicPorts = PutInstancePublicPorts'
  { -- | An array of objects to describe the ports to open for the specified
    -- instance.
    portInfos :: [PortInfo],
    -- | The name of the instance for which to open ports.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInstancePublicPorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portInfos', 'putInstancePublicPorts_portInfos' - An array of objects to describe the ports to open for the specified
-- instance.
--
-- 'instanceName', 'putInstancePublicPorts_instanceName' - The name of the instance for which to open ports.
newPutInstancePublicPorts ::
  -- | 'instanceName'
  Prelude.Text ->
  PutInstancePublicPorts
newPutInstancePublicPorts pInstanceName_ =
  PutInstancePublicPorts'
    { portInfos = Prelude.mempty,
      instanceName = pInstanceName_
    }

-- | An array of objects to describe the ports to open for the specified
-- instance.
putInstancePublicPorts_portInfos :: Lens.Lens' PutInstancePublicPorts [PortInfo]
putInstancePublicPorts_portInfos = Lens.lens (\PutInstancePublicPorts' {portInfos} -> portInfos) (\s@PutInstancePublicPorts' {} a -> s {portInfos = a} :: PutInstancePublicPorts) Prelude.. Lens.coerced

-- | The name of the instance for which to open ports.
putInstancePublicPorts_instanceName :: Lens.Lens' PutInstancePublicPorts Prelude.Text
putInstancePublicPorts_instanceName = Lens.lens (\PutInstancePublicPorts' {instanceName} -> instanceName) (\s@PutInstancePublicPorts' {} a -> s {instanceName = a} :: PutInstancePublicPorts)

instance Core.AWSRequest PutInstancePublicPorts where
  type
    AWSResponse PutInstancePublicPorts =
      PutInstancePublicPortsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutInstancePublicPortsResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutInstancePublicPorts where
  hashWithSalt _salt PutInstancePublicPorts' {..} =
    _salt
      `Prelude.hashWithSalt` portInfos
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData PutInstancePublicPorts where
  rnf PutInstancePublicPorts' {..} =
    Prelude.rnf portInfos
      `Prelude.seq` Prelude.rnf instanceName

instance Data.ToHeaders PutInstancePublicPorts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.PutInstancePublicPorts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutInstancePublicPorts where
  toJSON PutInstancePublicPorts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("portInfos" Data..= portInfos),
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath PutInstancePublicPorts where
  toPath = Prelude.const "/"

instance Data.ToQuery PutInstancePublicPorts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutInstancePublicPortsResponse' smart constructor.
data PutInstancePublicPortsResponse = PutInstancePublicPortsResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInstancePublicPortsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'putInstancePublicPortsResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'putInstancePublicPortsResponse_httpStatus' - The response's http status code.
newPutInstancePublicPortsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutInstancePublicPortsResponse
newPutInstancePublicPortsResponse pHttpStatus_ =
  PutInstancePublicPortsResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
putInstancePublicPortsResponse_operation :: Lens.Lens' PutInstancePublicPortsResponse (Prelude.Maybe Operation)
putInstancePublicPortsResponse_operation = Lens.lens (\PutInstancePublicPortsResponse' {operation} -> operation) (\s@PutInstancePublicPortsResponse' {} a -> s {operation = a} :: PutInstancePublicPortsResponse)

-- | The response's http status code.
putInstancePublicPortsResponse_httpStatus :: Lens.Lens' PutInstancePublicPortsResponse Prelude.Int
putInstancePublicPortsResponse_httpStatus = Lens.lens (\PutInstancePublicPortsResponse' {httpStatus} -> httpStatus) (\s@PutInstancePublicPortsResponse' {} a -> s {httpStatus = a} :: PutInstancePublicPortsResponse)

instance
  Prelude.NFData
    PutInstancePublicPortsResponse
  where
  rnf PutInstancePublicPortsResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
