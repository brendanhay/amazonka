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
-- Module      : Network.AWS.Organizations.ListDelegatedAdministrators
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS accounts that are designated as delegated administrators
-- in this organization.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedAdministrators
  ( -- * Creating a Request
    ListDelegatedAdministrators (..),
    newListDelegatedAdministrators,

    -- * Request Lenses
    listDelegatedAdministrators_servicePrincipal,
    listDelegatedAdministrators_nextToken,
    listDelegatedAdministrators_maxResults,

    -- * Destructuring the Response
    ListDelegatedAdministratorsResponse (..),
    newListDelegatedAdministratorsResponse,

    -- * Response Lenses
    listDelegatedAdministratorsResponse_nextToken,
    listDelegatedAdministratorsResponse_delegatedAdministrators,
    listDelegatedAdministratorsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDelegatedAdministrators' smart constructor.
data ListDelegatedAdministrators = ListDelegatedAdministrators'
  { -- | Specifies a service principal name. If specified, then the operation
    -- lists the delegated administrators only for the specified service.
    --
    -- If you don\'t specify a service principal, the operation lists all
    -- delegated administrators for all services in your organization.
    servicePrincipal :: Prelude.Maybe Prelude.Text,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ListDelegatedAdministrators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servicePrincipal', 'listDelegatedAdministrators_servicePrincipal' - Specifies a service principal name. If specified, then the operation
-- lists the delegated administrators only for the specified service.
--
-- If you don\'t specify a service principal, the operation lists all
-- delegated administrators for all services in your organization.
--
-- 'nextToken', 'listDelegatedAdministrators_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listDelegatedAdministrators_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
newListDelegatedAdministrators ::
  ListDelegatedAdministrators
newListDelegatedAdministrators =
  ListDelegatedAdministrators'
    { servicePrincipal =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Specifies a service principal name. If specified, then the operation
-- lists the delegated administrators only for the specified service.
--
-- If you don\'t specify a service principal, the operation lists all
-- delegated administrators for all services in your organization.
listDelegatedAdministrators_servicePrincipal :: Lens.Lens' ListDelegatedAdministrators (Prelude.Maybe Prelude.Text)
listDelegatedAdministrators_servicePrincipal = Lens.lens (\ListDelegatedAdministrators' {servicePrincipal} -> servicePrincipal) (\s@ListDelegatedAdministrators' {} a -> s {servicePrincipal = a} :: ListDelegatedAdministrators)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listDelegatedAdministrators_nextToken :: Lens.Lens' ListDelegatedAdministrators (Prelude.Maybe Prelude.Text)
listDelegatedAdministrators_nextToken = Lens.lens (\ListDelegatedAdministrators' {nextToken} -> nextToken) (\s@ListDelegatedAdministrators' {} a -> s {nextToken = a} :: ListDelegatedAdministrators)

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
listDelegatedAdministrators_maxResults :: Lens.Lens' ListDelegatedAdministrators (Prelude.Maybe Prelude.Natural)
listDelegatedAdministrators_maxResults = Lens.lens (\ListDelegatedAdministrators' {maxResults} -> maxResults) (\s@ListDelegatedAdministrators' {} a -> s {maxResults = a} :: ListDelegatedAdministrators)

instance Core.AWSPager ListDelegatedAdministrators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDelegatedAdministratorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDelegatedAdministratorsResponse_delegatedAdministrators
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDelegatedAdministrators_nextToken
          Lens..~ rs
          Lens.^? listDelegatedAdministratorsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDelegatedAdministrators where
  type
    AWSResponse ListDelegatedAdministrators =
      ListDelegatedAdministratorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDelegatedAdministratorsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DelegatedAdministrators"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDelegatedAdministrators

instance Prelude.NFData ListDelegatedAdministrators

instance Core.ToHeaders ListDelegatedAdministrators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListDelegatedAdministrators" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDelegatedAdministrators where
  toJSON ListDelegatedAdministrators' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServicePrincipal" Core..=)
              Prelude.<$> servicePrincipal,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDelegatedAdministrators where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDelegatedAdministrators where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDelegatedAdministratorsResponse' smart constructor.
data ListDelegatedAdministratorsResponse = ListDelegatedAdministratorsResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of delegated administrators in your organization.
    delegatedAdministrators :: Prelude.Maybe [DelegatedAdministrator],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDelegatedAdministratorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDelegatedAdministratorsResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'delegatedAdministrators', 'listDelegatedAdministratorsResponse_delegatedAdministrators' - The list of delegated administrators in your organization.
--
-- 'httpStatus', 'listDelegatedAdministratorsResponse_httpStatus' - The response's http status code.
newListDelegatedAdministratorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDelegatedAdministratorsResponse
newListDelegatedAdministratorsResponse pHttpStatus_ =
  ListDelegatedAdministratorsResponse'
    { nextToken =
        Prelude.Nothing,
      delegatedAdministrators =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listDelegatedAdministratorsResponse_nextToken :: Lens.Lens' ListDelegatedAdministratorsResponse (Prelude.Maybe Prelude.Text)
listDelegatedAdministratorsResponse_nextToken = Lens.lens (\ListDelegatedAdministratorsResponse' {nextToken} -> nextToken) (\s@ListDelegatedAdministratorsResponse' {} a -> s {nextToken = a} :: ListDelegatedAdministratorsResponse)

-- | The list of delegated administrators in your organization.
listDelegatedAdministratorsResponse_delegatedAdministrators :: Lens.Lens' ListDelegatedAdministratorsResponse (Prelude.Maybe [DelegatedAdministrator])
listDelegatedAdministratorsResponse_delegatedAdministrators = Lens.lens (\ListDelegatedAdministratorsResponse' {delegatedAdministrators} -> delegatedAdministrators) (\s@ListDelegatedAdministratorsResponse' {} a -> s {delegatedAdministrators = a} :: ListDelegatedAdministratorsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDelegatedAdministratorsResponse_httpStatus :: Lens.Lens' ListDelegatedAdministratorsResponse Prelude.Int
listDelegatedAdministratorsResponse_httpStatus = Lens.lens (\ListDelegatedAdministratorsResponse' {httpStatus} -> httpStatus) (\s@ListDelegatedAdministratorsResponse' {} a -> s {httpStatus = a} :: ListDelegatedAdministratorsResponse)

instance
  Prelude.NFData
    ListDelegatedAdministratorsResponse
