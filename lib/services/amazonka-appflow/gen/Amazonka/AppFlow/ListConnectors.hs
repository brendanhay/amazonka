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
-- Module      : Amazonka.AppFlow.ListConnectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of all registered custom connectors in your Amazon Web
-- Services account. This API lists only custom connectors registered in
-- this account, not the Amazon Web Services authored connectors.
module Amazonka.AppFlow.ListConnectors
  ( -- * Creating a Request
    ListConnectors (..),
    newListConnectors,

    -- * Request Lenses
    listConnectors_maxResults,
    listConnectors_nextToken,

    -- * Destructuring the Response
    ListConnectorsResponse (..),
    newListConnectorsResponse,

    -- * Response Lenses
    listConnectorsResponse_connectors,
    listConnectorsResponse_nextToken,
    listConnectorsResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnectors' smart constructor.
data ListConnectors = ListConnectors'
  { -- | Specifies the maximum number of items that should be returned in the
    -- result set. The default for @maxResults@ is 20 (for all paginated API
    -- operations).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the next page of data.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listConnectors_maxResults' - Specifies the maximum number of items that should be returned in the
-- result set. The default for @maxResults@ is 20 (for all paginated API
-- operations).
--
-- 'nextToken', 'listConnectors_nextToken' - The pagination token for the next page of data.
newListConnectors ::
  ListConnectors
newListConnectors =
  ListConnectors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifies the maximum number of items that should be returned in the
-- result set. The default for @maxResults@ is 20 (for all paginated API
-- operations).
listConnectors_maxResults :: Lens.Lens' ListConnectors (Prelude.Maybe Prelude.Natural)
listConnectors_maxResults = Lens.lens (\ListConnectors' {maxResults} -> maxResults) (\s@ListConnectors' {} a -> s {maxResults = a} :: ListConnectors)

-- | The pagination token for the next page of data.
listConnectors_nextToken :: Lens.Lens' ListConnectors (Prelude.Maybe Prelude.Text)
listConnectors_nextToken = Lens.lens (\ListConnectors' {nextToken} -> nextToken) (\s@ListConnectors' {} a -> s {nextToken = a} :: ListConnectors)

instance Core.AWSRequest ListConnectors where
  type
    AWSResponse ListConnectors =
      ListConnectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorsResponse'
            Prelude.<$> (x Data..?> "connectors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConnectors where
  hashWithSalt _salt ListConnectors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConnectors where
  rnf ListConnectors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListConnectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListConnectors where
  toJSON ListConnectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListConnectors where
  toPath = Prelude.const "/list-connectors"

instance Data.ToQuery ListConnectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConnectorsResponse' smart constructor.
data ListConnectorsResponse = ListConnectorsResponse'
  { -- | Contains information about the connectors supported by Amazon AppFlow.
    connectors :: Prelude.Maybe [ConnectorDetail],
    -- | The pagination token for the next page of data. If nextToken=null, this
    -- means that all records have been fetched.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectors', 'listConnectorsResponse_connectors' - Contains information about the connectors supported by Amazon AppFlow.
--
-- 'nextToken', 'listConnectorsResponse_nextToken' - The pagination token for the next page of data. If nextToken=null, this
-- means that all records have been fetched.
--
-- 'httpStatus', 'listConnectorsResponse_httpStatus' - The response's http status code.
newListConnectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectorsResponse
newListConnectorsResponse pHttpStatus_ =
  ListConnectorsResponse'
    { connectors =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about the connectors supported by Amazon AppFlow.
listConnectorsResponse_connectors :: Lens.Lens' ListConnectorsResponse (Prelude.Maybe [ConnectorDetail])
listConnectorsResponse_connectors = Lens.lens (\ListConnectorsResponse' {connectors} -> connectors) (\s@ListConnectorsResponse' {} a -> s {connectors = a} :: ListConnectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the next page of data. If nextToken=null, this
-- means that all records have been fetched.
listConnectorsResponse_nextToken :: Lens.Lens' ListConnectorsResponse (Prelude.Maybe Prelude.Text)
listConnectorsResponse_nextToken = Lens.lens (\ListConnectorsResponse' {nextToken} -> nextToken) (\s@ListConnectorsResponse' {} a -> s {nextToken = a} :: ListConnectorsResponse)

-- | The response's http status code.
listConnectorsResponse_httpStatus :: Lens.Lens' ListConnectorsResponse Prelude.Int
listConnectorsResponse_httpStatus = Lens.lens (\ListConnectorsResponse' {httpStatus} -> httpStatus) (\s@ListConnectorsResponse' {} a -> s {httpStatus = a} :: ListConnectorsResponse)

instance Prelude.NFData ListConnectorsResponse where
  rnf ListConnectorsResponse' {..} =
    Prelude.rnf connectors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
