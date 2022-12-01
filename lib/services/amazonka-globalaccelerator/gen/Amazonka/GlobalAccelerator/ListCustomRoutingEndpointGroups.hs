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
-- Module      : Amazonka.GlobalAccelerator.ListCustomRoutingEndpointGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the endpoint groups that are associated with a listener for a
-- custom routing accelerator.
module Amazonka.GlobalAccelerator.ListCustomRoutingEndpointGroups
  ( -- * Creating a Request
    ListCustomRoutingEndpointGroups (..),
    newListCustomRoutingEndpointGroups,

    -- * Request Lenses
    listCustomRoutingEndpointGroups_nextToken,
    listCustomRoutingEndpointGroups_maxResults,
    listCustomRoutingEndpointGroups_listenerArn,

    -- * Destructuring the Response
    ListCustomRoutingEndpointGroupsResponse (..),
    newListCustomRoutingEndpointGroupsResponse,

    -- * Response Lenses
    listCustomRoutingEndpointGroupsResponse_nextToken,
    listCustomRoutingEndpointGroupsResponse_endpointGroups,
    listCustomRoutingEndpointGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomRoutingEndpointGroups' smart constructor.
data ListCustomRoutingEndpointGroups = ListCustomRoutingEndpointGroups'
  { -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of endpoint group objects that you want to return with this
    -- call. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the listener to list endpoint groups
    -- for.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingEndpointGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomRoutingEndpointGroups_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'maxResults', 'listCustomRoutingEndpointGroups_maxResults' - The number of endpoint group objects that you want to return with this
-- call. The default value is 10.
--
-- 'listenerArn', 'listCustomRoutingEndpointGroups_listenerArn' - The Amazon Resource Name (ARN) of the listener to list endpoint groups
-- for.
newListCustomRoutingEndpointGroups ::
  -- | 'listenerArn'
  Prelude.Text ->
  ListCustomRoutingEndpointGroups
newListCustomRoutingEndpointGroups pListenerArn_ =
  ListCustomRoutingEndpointGroups'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      listenerArn = pListenerArn_
    }

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingEndpointGroups_nextToken :: Lens.Lens' ListCustomRoutingEndpointGroups (Prelude.Maybe Prelude.Text)
listCustomRoutingEndpointGroups_nextToken = Lens.lens (\ListCustomRoutingEndpointGroups' {nextToken} -> nextToken) (\s@ListCustomRoutingEndpointGroups' {} a -> s {nextToken = a} :: ListCustomRoutingEndpointGroups)

-- | The number of endpoint group objects that you want to return with this
-- call. The default value is 10.
listCustomRoutingEndpointGroups_maxResults :: Lens.Lens' ListCustomRoutingEndpointGroups (Prelude.Maybe Prelude.Natural)
listCustomRoutingEndpointGroups_maxResults = Lens.lens (\ListCustomRoutingEndpointGroups' {maxResults} -> maxResults) (\s@ListCustomRoutingEndpointGroups' {} a -> s {maxResults = a} :: ListCustomRoutingEndpointGroups)

-- | The Amazon Resource Name (ARN) of the listener to list endpoint groups
-- for.
listCustomRoutingEndpointGroups_listenerArn :: Lens.Lens' ListCustomRoutingEndpointGroups Prelude.Text
listCustomRoutingEndpointGroups_listenerArn = Lens.lens (\ListCustomRoutingEndpointGroups' {listenerArn} -> listenerArn) (\s@ListCustomRoutingEndpointGroups' {} a -> s {listenerArn = a} :: ListCustomRoutingEndpointGroups)

instance
  Core.AWSRequest
    ListCustomRoutingEndpointGroups
  where
  type
    AWSResponse ListCustomRoutingEndpointGroups =
      ListCustomRoutingEndpointGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomRoutingEndpointGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "EndpointGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomRoutingEndpointGroups
  where
  hashWithSalt
    _salt
    ListCustomRoutingEndpointGroups' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` listenerArn

instance
  Prelude.NFData
    ListCustomRoutingEndpointGroups
  where
  rnf ListCustomRoutingEndpointGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf listenerArn

instance
  Core.ToHeaders
    ListCustomRoutingEndpointGroups
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.ListCustomRoutingEndpointGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCustomRoutingEndpointGroups where
  toJSON ListCustomRoutingEndpointGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ListenerArn" Core..= listenerArn)
          ]
      )

instance Core.ToPath ListCustomRoutingEndpointGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCustomRoutingEndpointGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomRoutingEndpointGroupsResponse' smart constructor.
data ListCustomRoutingEndpointGroupsResponse = ListCustomRoutingEndpointGroupsResponse'
  { -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of the endpoint groups associated with a listener for a custom
    -- routing accelerator.
    endpointGroups :: Prelude.Maybe [CustomRoutingEndpointGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingEndpointGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomRoutingEndpointGroupsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'endpointGroups', 'listCustomRoutingEndpointGroupsResponse_endpointGroups' - The list of the endpoint groups associated with a listener for a custom
-- routing accelerator.
--
-- 'httpStatus', 'listCustomRoutingEndpointGroupsResponse_httpStatus' - The response's http status code.
newListCustomRoutingEndpointGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomRoutingEndpointGroupsResponse
newListCustomRoutingEndpointGroupsResponse
  pHttpStatus_ =
    ListCustomRoutingEndpointGroupsResponse'
      { nextToken =
          Prelude.Nothing,
        endpointGroups = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingEndpointGroupsResponse_nextToken :: Lens.Lens' ListCustomRoutingEndpointGroupsResponse (Prelude.Maybe Prelude.Text)
listCustomRoutingEndpointGroupsResponse_nextToken = Lens.lens (\ListCustomRoutingEndpointGroupsResponse' {nextToken} -> nextToken) (\s@ListCustomRoutingEndpointGroupsResponse' {} a -> s {nextToken = a} :: ListCustomRoutingEndpointGroupsResponse)

-- | The list of the endpoint groups associated with a listener for a custom
-- routing accelerator.
listCustomRoutingEndpointGroupsResponse_endpointGroups :: Lens.Lens' ListCustomRoutingEndpointGroupsResponse (Prelude.Maybe [CustomRoutingEndpointGroup])
listCustomRoutingEndpointGroupsResponse_endpointGroups = Lens.lens (\ListCustomRoutingEndpointGroupsResponse' {endpointGroups} -> endpointGroups) (\s@ListCustomRoutingEndpointGroupsResponse' {} a -> s {endpointGroups = a} :: ListCustomRoutingEndpointGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCustomRoutingEndpointGroupsResponse_httpStatus :: Lens.Lens' ListCustomRoutingEndpointGroupsResponse Prelude.Int
listCustomRoutingEndpointGroupsResponse_httpStatus = Lens.lens (\ListCustomRoutingEndpointGroupsResponse' {httpStatus} -> httpStatus) (\s@ListCustomRoutingEndpointGroupsResponse' {} a -> s {httpStatus = a} :: ListCustomRoutingEndpointGroupsResponse)

instance
  Prelude.NFData
    ListCustomRoutingEndpointGroupsResponse
  where
  rnf ListCustomRoutingEndpointGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endpointGroups
      `Prelude.seq` Prelude.rnf httpStatus
