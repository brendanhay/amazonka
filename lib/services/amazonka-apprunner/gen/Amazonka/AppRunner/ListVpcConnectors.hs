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
-- Module      : Amazonka.AppRunner.ListVpcConnectors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of App Runner VPC connectors in your Amazon Web Services
-- account.
module Amazonka.AppRunner.ListVpcConnectors
  ( -- * Creating a Request
    ListVpcConnectors (..),
    newListVpcConnectors,

    -- * Request Lenses
    listVpcConnectors_nextToken,
    listVpcConnectors_maxResults,

    -- * Destructuring the Response
    ListVpcConnectorsResponse (..),
    newListVpcConnectorsResponse,

    -- * Response Lenses
    listVpcConnectorsResponse_nextToken,
    listVpcConnectorsResponse_httpStatus,
    listVpcConnectorsResponse_vpcConnectors,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVpcConnectors' smart constructor.
data ListVpcConnectors = ListVpcConnectors'
  { -- | A token from a previous result page. It\'s used for a paginated request.
    -- The request retrieves the next result page. All other parameter values
    -- must be identical to the ones that are specified in the initial request.
    --
    -- If you don\'t specify @NextToken@, the request retrieves the first
    -- result page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in each response (result page).
    -- It\'s used for a paginated request.
    --
    -- If you don\'t specify @MaxResults@, the request retrieves all available
    -- results in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcConnectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcConnectors_nextToken' - A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones that are specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
--
-- 'maxResults', 'listVpcConnectors_maxResults' - The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
newListVpcConnectors ::
  ListVpcConnectors
newListVpcConnectors =
  ListVpcConnectors'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones that are specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
listVpcConnectors_nextToken :: Lens.Lens' ListVpcConnectors (Prelude.Maybe Prelude.Text)
listVpcConnectors_nextToken = Lens.lens (\ListVpcConnectors' {nextToken} -> nextToken) (\s@ListVpcConnectors' {} a -> s {nextToken = a} :: ListVpcConnectors)

-- | The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
listVpcConnectors_maxResults :: Lens.Lens' ListVpcConnectors (Prelude.Maybe Prelude.Natural)
listVpcConnectors_maxResults = Lens.lens (\ListVpcConnectors' {maxResults} -> maxResults) (\s@ListVpcConnectors' {} a -> s {maxResults = a} :: ListVpcConnectors)

instance Core.AWSRequest ListVpcConnectors where
  type
    AWSResponse ListVpcConnectors =
      ListVpcConnectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVpcConnectorsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "VpcConnectors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListVpcConnectors where
  hashWithSalt _salt ListVpcConnectors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListVpcConnectors where
  rnf ListVpcConnectors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListVpcConnectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AppRunner.ListVpcConnectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListVpcConnectors where
  toJSON ListVpcConnectors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListVpcConnectors where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVpcConnectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVpcConnectorsResponse' smart constructor.
data ListVpcConnectorsResponse = ListVpcConnectorsResponse'
  { -- | The token that you can pass in a subsequent request to get the next
    -- result page. It\'s returned in a paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of information records for VPC connectors. In a paginated
    -- request, the request returns up to @MaxResults@ records for each call.
    vpcConnectors :: [VpcConnector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcConnectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcConnectorsResponse_nextToken' - The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
--
-- 'httpStatus', 'listVpcConnectorsResponse_httpStatus' - The response's http status code.
--
-- 'vpcConnectors', 'listVpcConnectorsResponse_vpcConnectors' - A list of information records for VPC connectors. In a paginated
-- request, the request returns up to @MaxResults@ records for each call.
newListVpcConnectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVpcConnectorsResponse
newListVpcConnectorsResponse pHttpStatus_ =
  ListVpcConnectorsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      vpcConnectors = Prelude.mempty
    }

-- | The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
listVpcConnectorsResponse_nextToken :: Lens.Lens' ListVpcConnectorsResponse (Prelude.Maybe Prelude.Text)
listVpcConnectorsResponse_nextToken = Lens.lens (\ListVpcConnectorsResponse' {nextToken} -> nextToken) (\s@ListVpcConnectorsResponse' {} a -> s {nextToken = a} :: ListVpcConnectorsResponse)

-- | The response's http status code.
listVpcConnectorsResponse_httpStatus :: Lens.Lens' ListVpcConnectorsResponse Prelude.Int
listVpcConnectorsResponse_httpStatus = Lens.lens (\ListVpcConnectorsResponse' {httpStatus} -> httpStatus) (\s@ListVpcConnectorsResponse' {} a -> s {httpStatus = a} :: ListVpcConnectorsResponse)

-- | A list of information records for VPC connectors. In a paginated
-- request, the request returns up to @MaxResults@ records for each call.
listVpcConnectorsResponse_vpcConnectors :: Lens.Lens' ListVpcConnectorsResponse [VpcConnector]
listVpcConnectorsResponse_vpcConnectors = Lens.lens (\ListVpcConnectorsResponse' {vpcConnectors} -> vpcConnectors) (\s@ListVpcConnectorsResponse' {} a -> s {vpcConnectors = a} :: ListVpcConnectorsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListVpcConnectorsResponse where
  rnf ListVpcConnectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcConnectors
