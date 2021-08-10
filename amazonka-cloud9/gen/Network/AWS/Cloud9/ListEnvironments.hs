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
-- Module      : Network.AWS.Cloud9.ListEnvironments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of AWS Cloud9 development environment identifiers.
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.ListEnvironments
  ( -- * Creating a Request
    ListEnvironments (..),
    newListEnvironments,

    -- * Request Lenses
    listEnvironments_nextToken,
    listEnvironments_maxResults,

    -- * Destructuring the Response
    ListEnvironmentsResponse (..),
    newListEnvironmentsResponse,

    -- * Response Lenses
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_environmentIds,
    listEnvironmentsResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEnvironments' smart constructor.
data ListEnvironments = ListEnvironments'
  { -- | During a previous call, if there are more than 25 items in the list,
    -- only the first 25 items are returned, along with a unique string called
    -- a /next token/. To get the next batch of items in the list, call this
    -- operation again, adding the next token to the call. To get all of the
    -- items in the list, keep calling this operation with each subsequent next
    -- token that is returned, until no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of environments to get identifiers for.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironments_nextToken' - During a previous call, if there are more than 25 items in the list,
-- only the first 25 items are returned, along with a unique string called
-- a /next token/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
--
-- 'maxResults', 'listEnvironments_maxResults' - The maximum number of environments to get identifiers for.
newListEnvironments ::
  ListEnvironments
newListEnvironments =
  ListEnvironments'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | During a previous call, if there are more than 25 items in the list,
-- only the first 25 items are returned, along with a unique string called
-- a /next token/. To get the next batch of items in the list, call this
-- operation again, adding the next token to the call. To get all of the
-- items in the list, keep calling this operation with each subsequent next
-- token that is returned, until no more next tokens are returned.
listEnvironments_nextToken :: Lens.Lens' ListEnvironments (Prelude.Maybe Prelude.Text)
listEnvironments_nextToken = Lens.lens (\ListEnvironments' {nextToken} -> nextToken) (\s@ListEnvironments' {} a -> s {nextToken = a} :: ListEnvironments)

-- | The maximum number of environments to get identifiers for.
listEnvironments_maxResults :: Lens.Lens' ListEnvironments (Prelude.Maybe Prelude.Natural)
listEnvironments_maxResults = Lens.lens (\ListEnvironments' {maxResults} -> maxResults) (\s@ListEnvironments' {} a -> s {maxResults = a} :: ListEnvironments)

instance Core.AWSPager ListEnvironments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEnvironmentsResponse_environmentIds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEnvironments_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEnvironments where
  type
    AWSResponse ListEnvironments =
      ListEnvironmentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "environmentIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEnvironments

instance Prelude.NFData ListEnvironments

instance Core.ToHeaders ListEnvironments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.ListEnvironments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEnvironments where
  toJSON ListEnvironments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListEnvironments where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEnvironments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnvironmentsResponse' smart constructor.
data ListEnvironmentsResponse = ListEnvironmentsResponse'
  { -- | If there are more than 25 items in the list, only the first 25 items are
    -- returned, along with a unique string called a /next token/. To get the
    -- next batch of items in the list, call this operation again, adding the
    -- next token to the call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of environment identifiers.
    environmentIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentsResponse_nextToken' - If there are more than 25 items in the list, only the first 25 items are
-- returned, along with a unique string called a /next token/. To get the
-- next batch of items in the list, call this operation again, adding the
-- next token to the call.
--
-- 'environmentIds', 'listEnvironmentsResponse_environmentIds' - The list of environment identifiers.
--
-- 'httpStatus', 'listEnvironmentsResponse_httpStatus' - The response's http status code.
newListEnvironmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentsResponse
newListEnvironmentsResponse pHttpStatus_ =
  ListEnvironmentsResponse'
    { nextToken =
        Prelude.Nothing,
      environmentIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more than 25 items in the list, only the first 25 items are
-- returned, along with a unique string called a /next token/. To get the
-- next batch of items in the list, call this operation again, adding the
-- next token to the call.
listEnvironmentsResponse_nextToken :: Lens.Lens' ListEnvironmentsResponse (Prelude.Maybe Prelude.Text)
listEnvironmentsResponse_nextToken = Lens.lens (\ListEnvironmentsResponse' {nextToken} -> nextToken) (\s@ListEnvironmentsResponse' {} a -> s {nextToken = a} :: ListEnvironmentsResponse)

-- | The list of environment identifiers.
listEnvironmentsResponse_environmentIds :: Lens.Lens' ListEnvironmentsResponse (Prelude.Maybe [Prelude.Text])
listEnvironmentsResponse_environmentIds = Lens.lens (\ListEnvironmentsResponse' {environmentIds} -> environmentIds) (\s@ListEnvironmentsResponse' {} a -> s {environmentIds = a} :: ListEnvironmentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listEnvironmentsResponse_httpStatus :: Lens.Lens' ListEnvironmentsResponse Prelude.Int
listEnvironmentsResponse_httpStatus = Lens.lens (\ListEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentsResponse' {} a -> s {httpStatus = a} :: ListEnvironmentsResponse)

instance Prelude.NFData ListEnvironmentsResponse
