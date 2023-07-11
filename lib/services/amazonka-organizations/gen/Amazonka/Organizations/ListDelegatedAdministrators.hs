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
-- Module      : Amazonka.Organizations.ListDelegatedAdministrators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Web Services accounts that are designated as delegated
-- administrators in this organization.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- Amazon Web Services service.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListDelegatedAdministrators
  ( -- * Creating a Request
    ListDelegatedAdministrators (..),
    newListDelegatedAdministrators,

    -- * Request Lenses
    listDelegatedAdministrators_maxResults,
    listDelegatedAdministrators_nextToken,
    listDelegatedAdministrators_servicePrincipal,

    -- * Destructuring the Response
    ListDelegatedAdministratorsResponse (..),
    newListDelegatedAdministratorsResponse,

    -- * Response Lenses
    listDelegatedAdministratorsResponse_delegatedAdministrators,
    listDelegatedAdministratorsResponse_nextToken,
    listDelegatedAdministratorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDelegatedAdministrators' smart constructor.
data ListDelegatedAdministrators = ListDelegatedAdministrators'
  { -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a service principal name. If specified, then the operation
    -- lists the delegated administrators only for the specified service.
    --
    -- If you don\'t specify a service principal, the operation lists all
    -- delegated administrators for all services in your organization.
    servicePrincipal :: Prelude.Maybe Prelude.Text
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
--
-- 'nextToken', 'listDelegatedAdministrators_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'servicePrincipal', 'listDelegatedAdministrators_servicePrincipal' - Specifies a service principal name. If specified, then the operation
-- lists the delegated administrators only for the specified service.
--
-- If you don\'t specify a service principal, the operation lists all
-- delegated administrators for all services in your organization.
newListDelegatedAdministrators ::
  ListDelegatedAdministrators
newListDelegatedAdministrators =
  ListDelegatedAdministrators'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      servicePrincipal = Prelude.Nothing
    }

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

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listDelegatedAdministrators_nextToken :: Lens.Lens' ListDelegatedAdministrators (Prelude.Maybe Prelude.Text)
listDelegatedAdministrators_nextToken = Lens.lens (\ListDelegatedAdministrators' {nextToken} -> nextToken) (\s@ListDelegatedAdministrators' {} a -> s {nextToken = a} :: ListDelegatedAdministrators)

-- | Specifies a service principal name. If specified, then the operation
-- lists the delegated administrators only for the specified service.
--
-- If you don\'t specify a service principal, the operation lists all
-- delegated administrators for all services in your organization.
listDelegatedAdministrators_servicePrincipal :: Lens.Lens' ListDelegatedAdministrators (Prelude.Maybe Prelude.Text)
listDelegatedAdministrators_servicePrincipal = Lens.lens (\ListDelegatedAdministrators' {servicePrincipal} -> servicePrincipal) (\s@ListDelegatedAdministrators' {} a -> s {servicePrincipal = a} :: ListDelegatedAdministrators)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDelegatedAdministrators_nextToken
          Lens..~ rs
          Lens.^? listDelegatedAdministratorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDelegatedAdministrators where
  type
    AWSResponse ListDelegatedAdministrators =
      ListDelegatedAdministratorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDelegatedAdministratorsResponse'
            Prelude.<$> ( x
                            Data..?> "DelegatedAdministrators"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDelegatedAdministrators where
  hashWithSalt _salt ListDelegatedAdministrators' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` servicePrincipal

instance Prelude.NFData ListDelegatedAdministrators where
  rnf ListDelegatedAdministrators' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf servicePrincipal

instance Data.ToHeaders ListDelegatedAdministrators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListDelegatedAdministrators" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDelegatedAdministrators where
  toJSON ListDelegatedAdministrators' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ServicePrincipal" Data..=)
              Prelude.<$> servicePrincipal
          ]
      )

instance Data.ToPath ListDelegatedAdministrators where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDelegatedAdministrators where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDelegatedAdministratorsResponse' smart constructor.
data ListDelegatedAdministratorsResponse = ListDelegatedAdministratorsResponse'
  { -- | The list of delegated administrators in your organization.
    delegatedAdministrators :: Prelude.Maybe [DelegatedAdministrator],
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'delegatedAdministrators', 'listDelegatedAdministratorsResponse_delegatedAdministrators' - The list of delegated administrators in your organization.
--
-- 'nextToken', 'listDelegatedAdministratorsResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'httpStatus', 'listDelegatedAdministratorsResponse_httpStatus' - The response's http status code.
newListDelegatedAdministratorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDelegatedAdministratorsResponse
newListDelegatedAdministratorsResponse pHttpStatus_ =
  ListDelegatedAdministratorsResponse'
    { delegatedAdministrators =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of delegated administrators in your organization.
listDelegatedAdministratorsResponse_delegatedAdministrators :: Lens.Lens' ListDelegatedAdministratorsResponse (Prelude.Maybe [DelegatedAdministrator])
listDelegatedAdministratorsResponse_delegatedAdministrators = Lens.lens (\ListDelegatedAdministratorsResponse' {delegatedAdministrators} -> delegatedAdministrators) (\s@ListDelegatedAdministratorsResponse' {} a -> s {delegatedAdministrators = a} :: ListDelegatedAdministratorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listDelegatedAdministratorsResponse_nextToken :: Lens.Lens' ListDelegatedAdministratorsResponse (Prelude.Maybe Prelude.Text)
listDelegatedAdministratorsResponse_nextToken = Lens.lens (\ListDelegatedAdministratorsResponse' {nextToken} -> nextToken) (\s@ListDelegatedAdministratorsResponse' {} a -> s {nextToken = a} :: ListDelegatedAdministratorsResponse)

-- | The response's http status code.
listDelegatedAdministratorsResponse_httpStatus :: Lens.Lens' ListDelegatedAdministratorsResponse Prelude.Int
listDelegatedAdministratorsResponse_httpStatus = Lens.lens (\ListDelegatedAdministratorsResponse' {httpStatus} -> httpStatus) (\s@ListDelegatedAdministratorsResponse' {} a -> s {httpStatus = a} :: ListDelegatedAdministratorsResponse)

instance
  Prelude.NFData
    ListDelegatedAdministratorsResponse
  where
  rnf ListDelegatedAdministratorsResponse' {..} =
    Prelude.rnf delegatedAdministrators
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
