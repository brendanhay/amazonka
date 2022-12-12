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
-- Module      : Amazonka.Organizations.ListAWSServiceAccessForOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Amazon Web Services services that you enabled to
-- integrate with your organization. After a service on this list creates
-- the resources that it requires for the integration, it can perform
-- operations on your organization and its accounts.
--
-- For more information about integrating other services with
-- Organizations, including the list of services that currently work with
-- Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating Organizations with Other Amazon Web Services Services>
-- in the /Organizations User Guide./
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- Amazon Web Services service.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListAWSServiceAccessForOrganization
  ( -- * Creating a Request
    ListAWSServiceAccessForOrganization (..),
    newListAWSServiceAccessForOrganization,

    -- * Request Lenses
    listAWSServiceAccessForOrganization_maxResults,
    listAWSServiceAccessForOrganization_nextToken,

    -- * Destructuring the Response
    ListAWSServiceAccessForOrganizationResponse (..),
    newListAWSServiceAccessForOrganizationResponse,

    -- * Response Lenses
    listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals,
    listAWSServiceAccessForOrganizationResponse_nextToken,
    listAWSServiceAccessForOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAWSServiceAccessForOrganization' smart constructor.
data ListAWSServiceAccessForOrganization = ListAWSServiceAccessForOrganization'
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
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAWSServiceAccessForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAWSServiceAccessForOrganization_maxResults' - The total number of results that you want included on each page of the
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
-- 'nextToken', 'listAWSServiceAccessForOrganization_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
newListAWSServiceAccessForOrganization ::
  ListAWSServiceAccessForOrganization
newListAWSServiceAccessForOrganization =
  ListAWSServiceAccessForOrganization'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
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
listAWSServiceAccessForOrganization_maxResults :: Lens.Lens' ListAWSServiceAccessForOrganization (Prelude.Maybe Prelude.Natural)
listAWSServiceAccessForOrganization_maxResults = Lens.lens (\ListAWSServiceAccessForOrganization' {maxResults} -> maxResults) (\s@ListAWSServiceAccessForOrganization' {} a -> s {maxResults = a} :: ListAWSServiceAccessForOrganization)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listAWSServiceAccessForOrganization_nextToken :: Lens.Lens' ListAWSServiceAccessForOrganization (Prelude.Maybe Prelude.Text)
listAWSServiceAccessForOrganization_nextToken = Lens.lens (\ListAWSServiceAccessForOrganization' {nextToken} -> nextToken) (\s@ListAWSServiceAccessForOrganization' {} a -> s {nextToken = a} :: ListAWSServiceAccessForOrganization)

instance
  Core.AWSPager
    ListAWSServiceAccessForOrganization
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAWSServiceAccessForOrganizationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAWSServiceAccessForOrganization_nextToken
          Lens..~ rs
          Lens.^? listAWSServiceAccessForOrganizationResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAWSServiceAccessForOrganization
  where
  type
    AWSResponse ListAWSServiceAccessForOrganization =
      ListAWSServiceAccessForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAWSServiceAccessForOrganizationResponse'
            Prelude.<$> ( x Data..?> "EnabledServicePrincipals"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAWSServiceAccessForOrganization
  where
  hashWithSalt
    _salt
    ListAWSServiceAccessForOrganization' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListAWSServiceAccessForOrganization
  where
  rnf ListAWSServiceAccessForOrganization' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListAWSServiceAccessForOrganization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListAWSServiceAccessForOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListAWSServiceAccessForOrganization
  where
  toJSON ListAWSServiceAccessForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    ListAWSServiceAccessForOrganization
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListAWSServiceAccessForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAWSServiceAccessForOrganizationResponse' smart constructor.
data ListAWSServiceAccessForOrganizationResponse = ListAWSServiceAccessForOrganizationResponse'
  { -- | A list of the service principals for the services that are enabled to
    -- integrate with your organization. Each principal is a structure that
    -- includes the name and the date that it was enabled for integration with
    -- Organizations.
    enabledServicePrincipals :: Prelude.Maybe [EnabledServicePrincipal],
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAWSServiceAccessForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledServicePrincipals', 'listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals' - A list of the service principals for the services that are enabled to
-- integrate with your organization. Each principal is a structure that
-- includes the name and the date that it was enabled for integration with
-- Organizations.
--
-- 'nextToken', 'listAWSServiceAccessForOrganizationResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'httpStatus', 'listAWSServiceAccessForOrganizationResponse_httpStatus' - The response's http status code.
newListAWSServiceAccessForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAWSServiceAccessForOrganizationResponse
newListAWSServiceAccessForOrganizationResponse
  pHttpStatus_ =
    ListAWSServiceAccessForOrganizationResponse'
      { enabledServicePrincipals =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of the service principals for the services that are enabled to
-- integrate with your organization. Each principal is a structure that
-- includes the name and the date that it was enabled for integration with
-- Organizations.
listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse (Prelude.Maybe [EnabledServicePrincipal])
listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals = Lens.lens (\ListAWSServiceAccessForOrganizationResponse' {enabledServicePrincipals} -> enabledServicePrincipals) (\s@ListAWSServiceAccessForOrganizationResponse' {} a -> s {enabledServicePrincipals = a} :: ListAWSServiceAccessForOrganizationResponse) Prelude.. Lens.mapping Lens.coerced

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listAWSServiceAccessForOrganizationResponse_nextToken :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse (Prelude.Maybe Prelude.Text)
listAWSServiceAccessForOrganizationResponse_nextToken = Lens.lens (\ListAWSServiceAccessForOrganizationResponse' {nextToken} -> nextToken) (\s@ListAWSServiceAccessForOrganizationResponse' {} a -> s {nextToken = a} :: ListAWSServiceAccessForOrganizationResponse)

-- | The response's http status code.
listAWSServiceAccessForOrganizationResponse_httpStatus :: Lens.Lens' ListAWSServiceAccessForOrganizationResponse Prelude.Int
listAWSServiceAccessForOrganizationResponse_httpStatus = Lens.lens (\ListAWSServiceAccessForOrganizationResponse' {httpStatus} -> httpStatus) (\s@ListAWSServiceAccessForOrganizationResponse' {} a -> s {httpStatus = a} :: ListAWSServiceAccessForOrganizationResponse)

instance
  Prelude.NFData
    ListAWSServiceAccessForOrganizationResponse
  where
  rnf ListAWSServiceAccessForOrganizationResponse' {..} =
    Prelude.rnf enabledServicePrincipals
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
