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
-- Module      : Amazonka.AppFlow.DescribeConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given custom connector registered in your Amazon Web
-- Services account. This API can be used for custom connectors that are
-- registered in your account and also for Amazon authored connectors.
module Amazonka.AppFlow.DescribeConnector
  ( -- * Creating a Request
    DescribeConnector (..),
    newDescribeConnector,

    -- * Request Lenses
    describeConnector_connectorLabel,
    describeConnector_connectorType,

    -- * Destructuring the Response
    DescribeConnectorResponse (..),
    newDescribeConnectorResponse,

    -- * Response Lenses
    describeConnectorResponse_connectorConfiguration,
    describeConnectorResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConnector' smart constructor.
data DescribeConnector = DescribeConnector'
  { -- | The label of the connector. The label is unique for each
    -- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
    -- if calling for CUSTOMCONNECTOR connector type\/.
    connectorLabel :: Prelude.Maybe Prelude.Text,
    -- | The connector type, such as CUSTOMCONNECTOR, Saleforce, Marketo. Please
    -- choose CUSTOMCONNECTOR for Lambda based custom connectors.
    connectorType :: ConnectorType
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
-- 'connectorLabel', 'describeConnector_connectorLabel' - The label of the connector. The label is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
-- if calling for CUSTOMCONNECTOR connector type\/.
--
-- 'connectorType', 'describeConnector_connectorType' - The connector type, such as CUSTOMCONNECTOR, Saleforce, Marketo. Please
-- choose CUSTOMCONNECTOR for Lambda based custom connectors.
newDescribeConnector ::
  -- | 'connectorType'
  ConnectorType ->
  DescribeConnector
newDescribeConnector pConnectorType_ =
  DescribeConnector'
    { connectorLabel =
        Prelude.Nothing,
      connectorType = pConnectorType_
    }

-- | The label of the connector. The label is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
-- if calling for CUSTOMCONNECTOR connector type\/.
describeConnector_connectorLabel :: Lens.Lens' DescribeConnector (Prelude.Maybe Prelude.Text)
describeConnector_connectorLabel = Lens.lens (\DescribeConnector' {connectorLabel} -> connectorLabel) (\s@DescribeConnector' {} a -> s {connectorLabel = a} :: DescribeConnector)

-- | The connector type, such as CUSTOMCONNECTOR, Saleforce, Marketo. Please
-- choose CUSTOMCONNECTOR for Lambda based custom connectors.
describeConnector_connectorType :: Lens.Lens' DescribeConnector ConnectorType
describeConnector_connectorType = Lens.lens (\DescribeConnector' {connectorType} -> connectorType) (\s@DescribeConnector' {} a -> s {connectorType = a} :: DescribeConnector)

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
            Prelude.<$> (x Data..?> "connectorConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnector where
  hashWithSalt _salt DescribeConnector' {..} =
    _salt
      `Prelude.hashWithSalt` connectorLabel
      `Prelude.hashWithSalt` connectorType

instance Prelude.NFData DescribeConnector where
  rnf DescribeConnector' {..} =
    Prelude.rnf connectorLabel
      `Prelude.seq` Prelude.rnf connectorType

instance Data.ToHeaders DescribeConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConnector where
  toJSON DescribeConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectorLabel" Data..=)
              Prelude.<$> connectorLabel,
            Prelude.Just
              ("connectorType" Data..= connectorType)
          ]
      )

instance Data.ToPath DescribeConnector where
  toPath = Prelude.const "/describe-connector"

instance Data.ToQuery DescribeConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectorResponse' smart constructor.
data DescribeConnectorResponse = DescribeConnectorResponse'
  { -- | Configuration info of all the connectors that the user requested.
    connectorConfiguration :: Prelude.Maybe ConnectorConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'connectorConfiguration', 'describeConnectorResponse_connectorConfiguration' - Configuration info of all the connectors that the user requested.
--
-- 'httpStatus', 'describeConnectorResponse_httpStatus' - The response's http status code.
newDescribeConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectorResponse
newDescribeConnectorResponse pHttpStatus_ =
  DescribeConnectorResponse'
    { connectorConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Configuration info of all the connectors that the user requested.
describeConnectorResponse_connectorConfiguration :: Lens.Lens' DescribeConnectorResponse (Prelude.Maybe ConnectorConfiguration)
describeConnectorResponse_connectorConfiguration = Lens.lens (\DescribeConnectorResponse' {connectorConfiguration} -> connectorConfiguration) (\s@DescribeConnectorResponse' {} a -> s {connectorConfiguration = a} :: DescribeConnectorResponse)

-- | The response's http status code.
describeConnectorResponse_httpStatus :: Lens.Lens' DescribeConnectorResponse Prelude.Int
describeConnectorResponse_httpStatus = Lens.lens (\DescribeConnectorResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectorResponse' {} a -> s {httpStatus = a} :: DescribeConnectorResponse)

instance Prelude.NFData DescribeConnectorResponse where
  rnf DescribeConnectorResponse' {..} =
    Prelude.rnf connectorConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
