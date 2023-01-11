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
-- Module      : Amazonka.AppFlow.DescribeConnectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connectors vended by Amazon AppFlow for specified
-- connector types. If you don\'t specify a connector type, this operation
-- describes all connectors vended by Amazon AppFlow. If there are more
-- connectors than can be returned in one page, the response contains a
-- @nextToken@ object, which can be be passed in to the next call to the
-- @DescribeConnectors@ API operation to retrieve the next page.
module Amazonka.AppFlow.DescribeConnectors
  ( -- * Creating a Request
    DescribeConnectors (..),
    newDescribeConnectors,

    -- * Request Lenses
    describeConnectors_connectorTypes,
    describeConnectors_maxResults,
    describeConnectors_nextToken,

    -- * Destructuring the Response
    DescribeConnectorsResponse (..),
    newDescribeConnectorsResponse,

    -- * Response Lenses
    describeConnectorsResponse_connectorConfigurations,
    describeConnectorsResponse_connectors,
    describeConnectorsResponse_nextToken,
    describeConnectorsResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConnectors' smart constructor.
data DescribeConnectors = DescribeConnectors'
  { -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorTypes :: Prelude.Maybe [ConnectorType],
    -- | The maximum number of items that should be returned in the result set.
    -- The default is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the next page of data.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorTypes', 'describeConnectors_connectorTypes' - The type of connector, such as Salesforce, Amplitude, and so on.
--
-- 'maxResults', 'describeConnectors_maxResults' - The maximum number of items that should be returned in the result set.
-- The default is 20.
--
-- 'nextToken', 'describeConnectors_nextToken' - The pagination token for the next page of data.
newDescribeConnectors ::
  DescribeConnectors
newDescribeConnectors =
  DescribeConnectors'
    { connectorTypes =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The type of connector, such as Salesforce, Amplitude, and so on.
describeConnectors_connectorTypes :: Lens.Lens' DescribeConnectors (Prelude.Maybe [ConnectorType])
describeConnectors_connectorTypes = Lens.lens (\DescribeConnectors' {connectorTypes} -> connectorTypes) (\s@DescribeConnectors' {} a -> s {connectorTypes = a} :: DescribeConnectors) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items that should be returned in the result set.
-- The default is 20.
describeConnectors_maxResults :: Lens.Lens' DescribeConnectors (Prelude.Maybe Prelude.Natural)
describeConnectors_maxResults = Lens.lens (\DescribeConnectors' {maxResults} -> maxResults) (\s@DescribeConnectors' {} a -> s {maxResults = a} :: DescribeConnectors)

-- | The pagination token for the next page of data.
describeConnectors_nextToken :: Lens.Lens' DescribeConnectors (Prelude.Maybe Prelude.Text)
describeConnectors_nextToken = Lens.lens (\DescribeConnectors' {nextToken} -> nextToken) (\s@DescribeConnectors' {} a -> s {nextToken = a} :: DescribeConnectors)

instance Core.AWSRequest DescribeConnectors where
  type
    AWSResponse DescribeConnectors =
      DescribeConnectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectorsResponse'
            Prelude.<$> ( x Data..?> "connectorConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "connectors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnectors where
  hashWithSalt _salt DescribeConnectors' {..} =
    _salt `Prelude.hashWithSalt` connectorTypes
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeConnectors where
  rnf DescribeConnectors' {..} =
    Prelude.rnf connectorTypes
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeConnectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConnectors where
  toJSON DescribeConnectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectorTypes" Data..=)
              Prelude.<$> connectorTypes,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeConnectors where
  toPath = Prelude.const "/describe-connectors"

instance Data.ToQuery DescribeConnectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectorsResponse' smart constructor.
data DescribeConnectorsResponse = DescribeConnectorsResponse'
  { -- | The configuration that is applied to the connectors used in the flow.
    connectorConfigurations :: Prelude.Maybe (Prelude.HashMap ConnectorType ConnectorConfiguration),
    -- | Information about the connectors supported in Amazon AppFlow.
    connectors :: Prelude.Maybe [ConnectorDetail],
    -- | The pagination token for the next page of data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorConfigurations', 'describeConnectorsResponse_connectorConfigurations' - The configuration that is applied to the connectors used in the flow.
--
-- 'connectors', 'describeConnectorsResponse_connectors' - Information about the connectors supported in Amazon AppFlow.
--
-- 'nextToken', 'describeConnectorsResponse_nextToken' - The pagination token for the next page of data.
--
-- 'httpStatus', 'describeConnectorsResponse_httpStatus' - The response's http status code.
newDescribeConnectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectorsResponse
newDescribeConnectorsResponse pHttpStatus_ =
  DescribeConnectorsResponse'
    { connectorConfigurations =
        Prelude.Nothing,
      connectors = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration that is applied to the connectors used in the flow.
describeConnectorsResponse_connectorConfigurations :: Lens.Lens' DescribeConnectorsResponse (Prelude.Maybe (Prelude.HashMap ConnectorType ConnectorConfiguration))
describeConnectorsResponse_connectorConfigurations = Lens.lens (\DescribeConnectorsResponse' {connectorConfigurations} -> connectorConfigurations) (\s@DescribeConnectorsResponse' {} a -> s {connectorConfigurations = a} :: DescribeConnectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the connectors supported in Amazon AppFlow.
describeConnectorsResponse_connectors :: Lens.Lens' DescribeConnectorsResponse (Prelude.Maybe [ConnectorDetail])
describeConnectorsResponse_connectors = Lens.lens (\DescribeConnectorsResponse' {connectors} -> connectors) (\s@DescribeConnectorsResponse' {} a -> s {connectors = a} :: DescribeConnectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the next page of data.
describeConnectorsResponse_nextToken :: Lens.Lens' DescribeConnectorsResponse (Prelude.Maybe Prelude.Text)
describeConnectorsResponse_nextToken = Lens.lens (\DescribeConnectorsResponse' {nextToken} -> nextToken) (\s@DescribeConnectorsResponse' {} a -> s {nextToken = a} :: DescribeConnectorsResponse)

-- | The response's http status code.
describeConnectorsResponse_httpStatus :: Lens.Lens' DescribeConnectorsResponse Prelude.Int
describeConnectorsResponse_httpStatus = Lens.lens (\DescribeConnectorsResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectorsResponse' {} a -> s {httpStatus = a} :: DescribeConnectorsResponse)

instance Prelude.NFData DescribeConnectorsResponse where
  rnf DescribeConnectorsResponse' {..} =
    Prelude.rnf connectorConfigurations
      `Prelude.seq` Prelude.rnf connectors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
