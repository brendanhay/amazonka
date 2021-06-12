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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDelegatedAdministrators' smart constructor.
data ListDelegatedAdministrators = ListDelegatedAdministrators'
  { -- | Specifies a service principal name. If specified, then the operation
    -- lists the delegated administrators only for the specified service.
    --
    -- If you don\'t specify a service principal, the operation lists all
    -- delegated administrators for all services in your organization.
    servicePrincipal :: Core.Maybe Core.Text,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Core.Maybe Core.Text,
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
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Specifies a service principal name. If specified, then the operation
-- lists the delegated administrators only for the specified service.
--
-- If you don\'t specify a service principal, the operation lists all
-- delegated administrators for all services in your organization.
listDelegatedAdministrators_servicePrincipal :: Lens.Lens' ListDelegatedAdministrators (Core.Maybe Core.Text)
listDelegatedAdministrators_servicePrincipal = Lens.lens (\ListDelegatedAdministrators' {servicePrincipal} -> servicePrincipal) (\s@ListDelegatedAdministrators' {} a -> s {servicePrincipal = a} :: ListDelegatedAdministrators)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listDelegatedAdministrators_nextToken :: Lens.Lens' ListDelegatedAdministrators (Core.Maybe Core.Text)
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
listDelegatedAdministrators_maxResults :: Lens.Lens' ListDelegatedAdministrators (Core.Maybe Core.Natural)
listDelegatedAdministrators_maxResults = Lens.lens (\ListDelegatedAdministrators' {maxResults} -> maxResults) (\s@ListDelegatedAdministrators' {} a -> s {maxResults = a} :: ListDelegatedAdministrators)

instance Core.AWSPager ListDelegatedAdministrators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDelegatedAdministratorsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDelegatedAdministratorsResponse_delegatedAdministrators
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDelegatedAdministrators_nextToken
          Lens..~ rs
          Lens.^? listDelegatedAdministratorsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDelegatedAdministrators where
  type
    AWSResponse ListDelegatedAdministrators =
      ListDelegatedAdministratorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDelegatedAdministratorsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "DelegatedAdministrators"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDelegatedAdministrators

instance Core.NFData ListDelegatedAdministrators

instance Core.ToHeaders ListDelegatedAdministrators where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListDelegatedAdministrators" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDelegatedAdministrators where
  toJSON ListDelegatedAdministrators' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ServicePrincipal" Core..=)
              Core.<$> servicePrincipal,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListDelegatedAdministrators where
  toPath = Core.const "/"

instance Core.ToQuery ListDelegatedAdministrators where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDelegatedAdministratorsResponse' smart constructor.
data ListDelegatedAdministratorsResponse = ListDelegatedAdministratorsResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of delegated administrators in your organization.
    delegatedAdministrators :: Core.Maybe [DelegatedAdministrator],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  ListDelegatedAdministratorsResponse
newListDelegatedAdministratorsResponse pHttpStatus_ =
  ListDelegatedAdministratorsResponse'
    { nextToken =
        Core.Nothing,
      delegatedAdministrators = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listDelegatedAdministratorsResponse_nextToken :: Lens.Lens' ListDelegatedAdministratorsResponse (Core.Maybe Core.Text)
listDelegatedAdministratorsResponse_nextToken = Lens.lens (\ListDelegatedAdministratorsResponse' {nextToken} -> nextToken) (\s@ListDelegatedAdministratorsResponse' {} a -> s {nextToken = a} :: ListDelegatedAdministratorsResponse)

-- | The list of delegated administrators in your organization.
listDelegatedAdministratorsResponse_delegatedAdministrators :: Lens.Lens' ListDelegatedAdministratorsResponse (Core.Maybe [DelegatedAdministrator])
listDelegatedAdministratorsResponse_delegatedAdministrators = Lens.lens (\ListDelegatedAdministratorsResponse' {delegatedAdministrators} -> delegatedAdministrators) (\s@ListDelegatedAdministratorsResponse' {} a -> s {delegatedAdministrators = a} :: ListDelegatedAdministratorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDelegatedAdministratorsResponse_httpStatus :: Lens.Lens' ListDelegatedAdministratorsResponse Core.Int
listDelegatedAdministratorsResponse_httpStatus = Lens.lens (\ListDelegatedAdministratorsResponse' {httpStatus} -> httpStatus) (\s@ListDelegatedAdministratorsResponse' {} a -> s {httpStatus = a} :: ListDelegatedAdministratorsResponse)

instance
  Core.NFData
    ListDelegatedAdministratorsResponse
