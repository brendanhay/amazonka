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
-- Module      : Amazonka.GlobalAccelerator.ListEndpointGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the endpoint groups that are associated with a listener.
--
-- This operation returns paginated results.
module Amazonka.GlobalAccelerator.ListEndpointGroups
  ( -- * Creating a Request
    ListEndpointGroups (..),
    newListEndpointGroups,

    -- * Request Lenses
    listEndpointGroups_maxResults,
    listEndpointGroups_nextToken,
    listEndpointGroups_listenerArn,

    -- * Destructuring the Response
    ListEndpointGroupsResponse (..),
    newListEndpointGroupsResponse,

    -- * Response Lenses
    listEndpointGroupsResponse_endpointGroups,
    listEndpointGroupsResponse_nextToken,
    listEndpointGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEndpointGroups' smart constructor.
data ListEndpointGroups = ListEndpointGroups'
  { -- | The number of endpoint group objects that you want to return with this
    -- call. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEndpointGroups_maxResults' - The number of endpoint group objects that you want to return with this
-- call. The default value is 10.
--
-- 'nextToken', 'listEndpointGroups_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'listenerArn', 'listEndpointGroups_listenerArn' - The Amazon Resource Name (ARN) of the listener.
newListEndpointGroups ::
  -- | 'listenerArn'
  Prelude.Text ->
  ListEndpointGroups
newListEndpointGroups pListenerArn_ =
  ListEndpointGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      listenerArn = pListenerArn_
    }

-- | The number of endpoint group objects that you want to return with this
-- call. The default value is 10.
listEndpointGroups_maxResults :: Lens.Lens' ListEndpointGroups (Prelude.Maybe Prelude.Natural)
listEndpointGroups_maxResults = Lens.lens (\ListEndpointGroups' {maxResults} -> maxResults) (\s@ListEndpointGroups' {} a -> s {maxResults = a} :: ListEndpointGroups)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listEndpointGroups_nextToken :: Lens.Lens' ListEndpointGroups (Prelude.Maybe Prelude.Text)
listEndpointGroups_nextToken = Lens.lens (\ListEndpointGroups' {nextToken} -> nextToken) (\s@ListEndpointGroups' {} a -> s {nextToken = a} :: ListEndpointGroups)

-- | The Amazon Resource Name (ARN) of the listener.
listEndpointGroups_listenerArn :: Lens.Lens' ListEndpointGroups Prelude.Text
listEndpointGroups_listenerArn = Lens.lens (\ListEndpointGroups' {listenerArn} -> listenerArn) (\s@ListEndpointGroups' {} a -> s {listenerArn = a} :: ListEndpointGroups)

instance Core.AWSPager ListEndpointGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEndpointGroupsResponse_endpointGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEndpointGroups_nextToken
          Lens..~ rs
          Lens.^? listEndpointGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEndpointGroups where
  type
    AWSResponse ListEndpointGroups =
      ListEndpointGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointGroupsResponse'
            Prelude.<$> (x Data..?> "EndpointGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEndpointGroups where
  hashWithSalt _salt ListEndpointGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` listenerArn

instance Prelude.NFData ListEndpointGroups where
  rnf ListEndpointGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf listenerArn

instance Data.ToHeaders ListEndpointGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.ListEndpointGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEndpointGroups where
  toJSON ListEndpointGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ListenerArn" Data..= listenerArn)
          ]
      )

instance Data.ToPath ListEndpointGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEndpointGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEndpointGroupsResponse' smart constructor.
data ListEndpointGroupsResponse = ListEndpointGroupsResponse'
  { -- | The list of the endpoint groups associated with a listener.
    endpointGroups :: Prelude.Maybe [EndpointGroup],
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroups', 'listEndpointGroupsResponse_endpointGroups' - The list of the endpoint groups associated with a listener.
--
-- 'nextToken', 'listEndpointGroupsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'httpStatus', 'listEndpointGroupsResponse_httpStatus' - The response's http status code.
newListEndpointGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointGroupsResponse
newListEndpointGroupsResponse pHttpStatus_ =
  ListEndpointGroupsResponse'
    { endpointGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of the endpoint groups associated with a listener.
listEndpointGroupsResponse_endpointGroups :: Lens.Lens' ListEndpointGroupsResponse (Prelude.Maybe [EndpointGroup])
listEndpointGroupsResponse_endpointGroups = Lens.lens (\ListEndpointGroupsResponse' {endpointGroups} -> endpointGroups) (\s@ListEndpointGroupsResponse' {} a -> s {endpointGroups = a} :: ListEndpointGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. You receive this token from a
-- previous call.
listEndpointGroupsResponse_nextToken :: Lens.Lens' ListEndpointGroupsResponse (Prelude.Maybe Prelude.Text)
listEndpointGroupsResponse_nextToken = Lens.lens (\ListEndpointGroupsResponse' {nextToken} -> nextToken) (\s@ListEndpointGroupsResponse' {} a -> s {nextToken = a} :: ListEndpointGroupsResponse)

-- | The response's http status code.
listEndpointGroupsResponse_httpStatus :: Lens.Lens' ListEndpointGroupsResponse Prelude.Int
listEndpointGroupsResponse_httpStatus = Lens.lens (\ListEndpointGroupsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointGroupsResponse' {} a -> s {httpStatus = a} :: ListEndpointGroupsResponse)

instance Prelude.NFData ListEndpointGroupsResponse where
  rnf ListEndpointGroupsResponse' {..} =
    Prelude.rnf endpointGroups
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
