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
-- Module      : Network.AWS.Organizations.ListCreateAccountStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account creation requests that match the specified status that
-- is currently being tracked for the organization.
--
-- Always check the @NextToken@ response parameter for a @null@ value when
-- calling a @List*@ operation. These operations can occasionally return an
-- empty set of results even when there are more results available. The
-- @NextToken@ response parameter value is @null@ /only/ when there are no
-- more results to display.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListCreateAccountStatus
  ( -- * Creating a Request
    ListCreateAccountStatus (..),
    newListCreateAccountStatus,

    -- * Request Lenses
    listCreateAccountStatus_nextToken,
    listCreateAccountStatus_states,
    listCreateAccountStatus_maxResults,

    -- * Destructuring the Response
    ListCreateAccountStatusResponse (..),
    newListCreateAccountStatusResponse,

    -- * Response Lenses
    listCreateAccountStatusResponse_nextToken,
    listCreateAccountStatusResponse_createAccountStatuses,
    listCreateAccountStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCreateAccountStatus' smart constructor.
data ListCreateAccountStatus = ListCreateAccountStatus'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of one or more states that you want included in the response. If
    -- this parameter isn\'t present, all requests are included in the
    -- response.
    states :: Prelude.Maybe [CreateAccountState],
    -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCreateAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCreateAccountStatus_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'states', 'listCreateAccountStatus_states' - A list of one or more states that you want included in the response. If
-- this parameter isn\'t present, all requests are included in the
-- response.
--
-- 'maxResults', 'listCreateAccountStatus_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
newListCreateAccountStatus ::
  ListCreateAccountStatus
newListCreateAccountStatus =
  ListCreateAccountStatus'
    { nextToken =
        Prelude.Nothing,
      states = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listCreateAccountStatus_nextToken :: Lens.Lens' ListCreateAccountStatus (Prelude.Maybe Prelude.Text)
listCreateAccountStatus_nextToken = Lens.lens (\ListCreateAccountStatus' {nextToken} -> nextToken) (\s@ListCreateAccountStatus' {} a -> s {nextToken = a} :: ListCreateAccountStatus)

-- | A list of one or more states that you want included in the response. If
-- this parameter isn\'t present, all requests are included in the
-- response.
listCreateAccountStatus_states :: Lens.Lens' ListCreateAccountStatus (Prelude.Maybe [CreateAccountState])
listCreateAccountStatus_states = Lens.lens (\ListCreateAccountStatus' {states} -> states) (\s@ListCreateAccountStatus' {} a -> s {states = a} :: ListCreateAccountStatus) Prelude.. Lens.mapping Lens._Coerce

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listCreateAccountStatus_maxResults :: Lens.Lens' ListCreateAccountStatus (Prelude.Maybe Prelude.Natural)
listCreateAccountStatus_maxResults = Lens.lens (\ListCreateAccountStatus' {maxResults} -> maxResults) (\s@ListCreateAccountStatus' {} a -> s {maxResults = a} :: ListCreateAccountStatus)

instance Core.AWSPager ListCreateAccountStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCreateAccountStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCreateAccountStatusResponse_createAccountStatuses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCreateAccountStatus_nextToken
          Lens..~ rs
          Lens.^? listCreateAccountStatusResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCreateAccountStatus where
  type
    AWSResponse ListCreateAccountStatus =
      ListCreateAccountStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCreateAccountStatusResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CreateAccountStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCreateAccountStatus

instance Prelude.NFData ListCreateAccountStatus

instance Core.ToHeaders ListCreateAccountStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListCreateAccountStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCreateAccountStatus where
  toJSON ListCreateAccountStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("States" Core..=) Prelude.<$> states,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListCreateAccountStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCreateAccountStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCreateAccountStatusResponse' smart constructor.
data ListCreateAccountStatusResponse = ListCreateAccountStatusResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects with details about the requests. Certain elements,
    -- such as the accountId number, are present in the output only after the
    -- account has been successfully created.
    createAccountStatuses :: Prelude.Maybe [CreateAccountStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCreateAccountStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCreateAccountStatusResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'createAccountStatuses', 'listCreateAccountStatusResponse_createAccountStatuses' - A list of objects with details about the requests. Certain elements,
-- such as the accountId number, are present in the output only after the
-- account has been successfully created.
--
-- 'httpStatus', 'listCreateAccountStatusResponse_httpStatus' - The response's http status code.
newListCreateAccountStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCreateAccountStatusResponse
newListCreateAccountStatusResponse pHttpStatus_ =
  ListCreateAccountStatusResponse'
    { nextToken =
        Prelude.Nothing,
      createAccountStatuses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listCreateAccountStatusResponse_nextToken :: Lens.Lens' ListCreateAccountStatusResponse (Prelude.Maybe Prelude.Text)
listCreateAccountStatusResponse_nextToken = Lens.lens (\ListCreateAccountStatusResponse' {nextToken} -> nextToken) (\s@ListCreateAccountStatusResponse' {} a -> s {nextToken = a} :: ListCreateAccountStatusResponse)

-- | A list of objects with details about the requests. Certain elements,
-- such as the accountId number, are present in the output only after the
-- account has been successfully created.
listCreateAccountStatusResponse_createAccountStatuses :: Lens.Lens' ListCreateAccountStatusResponse (Prelude.Maybe [CreateAccountStatus])
listCreateAccountStatusResponse_createAccountStatuses = Lens.lens (\ListCreateAccountStatusResponse' {createAccountStatuses} -> createAccountStatuses) (\s@ListCreateAccountStatusResponse' {} a -> s {createAccountStatuses = a} :: ListCreateAccountStatusResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCreateAccountStatusResponse_httpStatus :: Lens.Lens' ListCreateAccountStatusResponse Prelude.Int
listCreateAccountStatusResponse_httpStatus = Lens.lens (\ListCreateAccountStatusResponse' {httpStatus} -> httpStatus) (\s@ListCreateAccountStatusResponse' {} a -> s {httpStatus = a} :: ListCreateAccountStatusResponse)

instance
  Prelude.NFData
    ListCreateAccountStatusResponse
