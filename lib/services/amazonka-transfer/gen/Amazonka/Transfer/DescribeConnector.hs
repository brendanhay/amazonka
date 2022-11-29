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
-- Module      : Amazonka.Transfer.DescribeConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connector that\'s identified by the @ConnectorId.@
module Amazonka.Transfer.DescribeConnector
  ( -- * Creating a Request
    DescribeConnector (..),
    newDescribeConnector,

    -- * Request Lenses
    describeConnector_connectorId,

    -- * Destructuring the Response
    DescribeConnectorResponse (..),
    newDescribeConnectorResponse,

    -- * Response Lenses
    describeConnectorResponse_httpStatus,
    describeConnectorResponse_connector,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeConnector' smart constructor.
data DescribeConnector = DescribeConnector'
  { -- | The unique identifier for the connector.
    connectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorId', 'describeConnector_connectorId' - The unique identifier for the connector.
newDescribeConnector ::
  -- | 'connectorId'
  Prelude.Text ->
  DescribeConnector
newDescribeConnector pConnectorId_ =
  DescribeConnector' {connectorId = pConnectorId_}

-- | The unique identifier for the connector.
describeConnector_connectorId :: Lens.Lens' DescribeConnector Prelude.Text
describeConnector_connectorId = Lens.lens (\DescribeConnector' {connectorId} -> connectorId) (\s@DescribeConnector' {} a -> s {connectorId = a} :: DescribeConnector)

instance Core.AWSRequest DescribeConnector where
  type
    AWSResponse DescribeConnector =
      DescribeConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Connector")
      )

instance Prelude.Hashable DescribeConnector where
  hashWithSalt _salt DescribeConnector' {..} =
    _salt `Prelude.hashWithSalt` connectorId

instance Prelude.NFData DescribeConnector where
  rnf DescribeConnector' {..} = Prelude.rnf connectorId

instance Core.ToHeaders DescribeConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.DescribeConnector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeConnector where
  toJSON DescribeConnector' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ConnectorId" Core..= connectorId)]
      )

instance Core.ToPath DescribeConnector where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectorResponse' smart constructor.
data DescribeConnectorResponse = DescribeConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The structure that contains the details of the connector.
    connector :: DescribedConnector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeConnectorResponse_httpStatus' - The response's http status code.
--
-- 'connector', 'describeConnectorResponse_connector' - The structure that contains the details of the connector.
newDescribeConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'connector'
  DescribedConnector ->
  DescribeConnectorResponse
newDescribeConnectorResponse pHttpStatus_ pConnector_ =
  DescribeConnectorResponse'
    { httpStatus =
        pHttpStatus_,
      connector = pConnector_
    }

-- | The response's http status code.
describeConnectorResponse_httpStatus :: Lens.Lens' DescribeConnectorResponse Prelude.Int
describeConnectorResponse_httpStatus = Lens.lens (\DescribeConnectorResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectorResponse' {} a -> s {httpStatus = a} :: DescribeConnectorResponse)

-- | The structure that contains the details of the connector.
describeConnectorResponse_connector :: Lens.Lens' DescribeConnectorResponse DescribedConnector
describeConnectorResponse_connector = Lens.lens (\DescribeConnectorResponse' {connector} -> connector) (\s@DescribeConnectorResponse' {} a -> s {connector = a} :: DescribeConnectorResponse)

instance Prelude.NFData DescribeConnectorResponse where
  rnf DescribeConnectorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf connector
