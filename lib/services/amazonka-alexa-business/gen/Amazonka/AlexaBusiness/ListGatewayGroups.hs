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
-- Module      : Amazonka.AlexaBusiness.ListGatewayGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway group summaries. Use GetGatewayGroup to
-- retrieve details of a specific gateway group.
module Amazonka.AlexaBusiness.ListGatewayGroups
  ( -- * Creating a Request
    ListGatewayGroups (..),
    newListGatewayGroups,

    -- * Request Lenses
    listGatewayGroups_nextToken,
    listGatewayGroups_maxResults,

    -- * Destructuring the Response
    ListGatewayGroupsResponse (..),
    newListGatewayGroupsResponse,

    -- * Response Lenses
    listGatewayGroupsResponse_nextToken,
    listGatewayGroupsResponse_gatewayGroups,
    listGatewayGroupsResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGatewayGroups' smart constructor.
data ListGatewayGroups = ListGatewayGroups'
  { -- | The token used to paginate though multiple pages of gateway group
    -- summaries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of gateway group summaries to return. The default is
    -- 50.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGatewayGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGatewayGroups_nextToken' - The token used to paginate though multiple pages of gateway group
-- summaries.
--
-- 'maxResults', 'listGatewayGroups_maxResults' - The maximum number of gateway group summaries to return. The default is
-- 50.
newListGatewayGroups ::
  ListGatewayGroups
newListGatewayGroups =
  ListGatewayGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token used to paginate though multiple pages of gateway group
-- summaries.
listGatewayGroups_nextToken :: Lens.Lens' ListGatewayGroups (Prelude.Maybe Prelude.Text)
listGatewayGroups_nextToken = Lens.lens (\ListGatewayGroups' {nextToken} -> nextToken) (\s@ListGatewayGroups' {} a -> s {nextToken = a} :: ListGatewayGroups)

-- | The maximum number of gateway group summaries to return. The default is
-- 50.
listGatewayGroups_maxResults :: Lens.Lens' ListGatewayGroups (Prelude.Maybe Prelude.Natural)
listGatewayGroups_maxResults = Lens.lens (\ListGatewayGroups' {maxResults} -> maxResults) (\s@ListGatewayGroups' {} a -> s {maxResults = a} :: ListGatewayGroups)

instance Core.AWSRequest ListGatewayGroups where
  type
    AWSResponse ListGatewayGroups =
      ListGatewayGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewayGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "GatewayGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGatewayGroups where
  hashWithSalt _salt ListGatewayGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListGatewayGroups where
  rnf ListGatewayGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListGatewayGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListGatewayGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListGatewayGroups where
  toJSON ListGatewayGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListGatewayGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListGatewayGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGatewayGroupsResponse' smart constructor.
data ListGatewayGroupsResponse = ListGatewayGroupsResponse'
  { -- | The token used to paginate though multiple pages of gateway group
    -- summaries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The gateway groups in the list.
    gatewayGroups :: Prelude.Maybe [GatewayGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGatewayGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGatewayGroupsResponse_nextToken' - The token used to paginate though multiple pages of gateway group
-- summaries.
--
-- 'gatewayGroups', 'listGatewayGroupsResponse_gatewayGroups' - The gateway groups in the list.
--
-- 'httpStatus', 'listGatewayGroupsResponse_httpStatus' - The response's http status code.
newListGatewayGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGatewayGroupsResponse
newListGatewayGroupsResponse pHttpStatus_ =
  ListGatewayGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      gatewayGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to paginate though multiple pages of gateway group
-- summaries.
listGatewayGroupsResponse_nextToken :: Lens.Lens' ListGatewayGroupsResponse (Prelude.Maybe Prelude.Text)
listGatewayGroupsResponse_nextToken = Lens.lens (\ListGatewayGroupsResponse' {nextToken} -> nextToken) (\s@ListGatewayGroupsResponse' {} a -> s {nextToken = a} :: ListGatewayGroupsResponse)

-- | The gateway groups in the list.
listGatewayGroupsResponse_gatewayGroups :: Lens.Lens' ListGatewayGroupsResponse (Prelude.Maybe [GatewayGroupSummary])
listGatewayGroupsResponse_gatewayGroups = Lens.lens (\ListGatewayGroupsResponse' {gatewayGroups} -> gatewayGroups) (\s@ListGatewayGroupsResponse' {} a -> s {gatewayGroups = a} :: ListGatewayGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGatewayGroupsResponse_httpStatus :: Lens.Lens' ListGatewayGroupsResponse Prelude.Int
listGatewayGroupsResponse_httpStatus = Lens.lens (\ListGatewayGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGatewayGroupsResponse' {} a -> s {httpStatus = a} :: ListGatewayGroupsResponse)

instance Prelude.NFData ListGatewayGroupsResponse where
  rnf ListGatewayGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gatewayGroups
      `Prelude.seq` Prelude.rnf httpStatus
