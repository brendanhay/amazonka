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
-- Module      : Amazonka.WorkMail.ListMailDomains
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mail domains in a given WorkMail organization.
module Amazonka.WorkMail.ListMailDomains
  ( -- * Creating a Request
    ListMailDomains (..),
    newListMailDomains,

    -- * Request Lenses
    listMailDomains_nextToken,
    listMailDomains_maxResults,
    listMailDomains_organizationId,

    -- * Destructuring the Response
    ListMailDomainsResponse (..),
    newListMailDomainsResponse,

    -- * Response Lenses
    listMailDomainsResponse_nextToken,
    listMailDomainsResponse_mailDomains,
    listMailDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListMailDomains' smart constructor.
data ListMailDomains = ListMailDomains'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not require a token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The WorkMail organization for which to list domains.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMailDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMailDomains_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not require a token.
--
-- 'maxResults', 'listMailDomains_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listMailDomains_organizationId' - The WorkMail organization for which to list domains.
newListMailDomains ::
  -- | 'organizationId'
  Prelude.Text ->
  ListMailDomains
newListMailDomains pOrganizationId_ =
  ListMailDomains'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not require a token.
listMailDomains_nextToken :: Lens.Lens' ListMailDomains (Prelude.Maybe Prelude.Text)
listMailDomains_nextToken = Lens.lens (\ListMailDomains' {nextToken} -> nextToken) (\s@ListMailDomains' {} a -> s {nextToken = a} :: ListMailDomains)

-- | The maximum number of results to return in a single call.
listMailDomains_maxResults :: Lens.Lens' ListMailDomains (Prelude.Maybe Prelude.Natural)
listMailDomains_maxResults = Lens.lens (\ListMailDomains' {maxResults} -> maxResults) (\s@ListMailDomains' {} a -> s {maxResults = a} :: ListMailDomains)

-- | The WorkMail organization for which to list domains.
listMailDomains_organizationId :: Lens.Lens' ListMailDomains Prelude.Text
listMailDomains_organizationId = Lens.lens (\ListMailDomains' {organizationId} -> organizationId) (\s@ListMailDomains' {} a -> s {organizationId = a} :: ListMailDomains)

instance Core.AWSRequest ListMailDomains where
  type
    AWSResponse ListMailDomains =
      ListMailDomainsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMailDomainsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "MailDomains" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMailDomains where
  hashWithSalt _salt ListMailDomains' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData ListMailDomains where
  rnf ListMailDomains' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf organizationId

instance Data.ToHeaders ListMailDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListMailDomains" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMailDomains where
  toJSON ListMailDomains' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath ListMailDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMailDomains where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMailDomainsResponse' smart constructor.
data ListMailDomainsResponse = ListMailDomainsResponse'
  { -- | The token to use to retrieve the next page of results. The value becomes
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of mail domain summaries, specifying domains that exist in the
    -- specified WorkMail organization, along with the information about
    -- whether the domain is or isn\'t the default.
    mailDomains :: Prelude.Maybe [MailDomainSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMailDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMailDomainsResponse_nextToken' - The token to use to retrieve the next page of results. The value becomes
-- @null@ when there are no more results to return.
--
-- 'mailDomains', 'listMailDomainsResponse_mailDomains' - The list of mail domain summaries, specifying domains that exist in the
-- specified WorkMail organization, along with the information about
-- whether the domain is or isn\'t the default.
--
-- 'httpStatus', 'listMailDomainsResponse_httpStatus' - The response's http status code.
newListMailDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMailDomainsResponse
newListMailDomainsResponse pHttpStatus_ =
  ListMailDomainsResponse'
    { nextToken =
        Prelude.Nothing,
      mailDomains = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value becomes
-- @null@ when there are no more results to return.
listMailDomainsResponse_nextToken :: Lens.Lens' ListMailDomainsResponse (Prelude.Maybe Prelude.Text)
listMailDomainsResponse_nextToken = Lens.lens (\ListMailDomainsResponse' {nextToken} -> nextToken) (\s@ListMailDomainsResponse' {} a -> s {nextToken = a} :: ListMailDomainsResponse)

-- | The list of mail domain summaries, specifying domains that exist in the
-- specified WorkMail organization, along with the information about
-- whether the domain is or isn\'t the default.
listMailDomainsResponse_mailDomains :: Lens.Lens' ListMailDomainsResponse (Prelude.Maybe [MailDomainSummary])
listMailDomainsResponse_mailDomains = Lens.lens (\ListMailDomainsResponse' {mailDomains} -> mailDomains) (\s@ListMailDomainsResponse' {} a -> s {mailDomains = a} :: ListMailDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMailDomainsResponse_httpStatus :: Lens.Lens' ListMailDomainsResponse Prelude.Int
listMailDomainsResponse_httpStatus = Lens.lens (\ListMailDomainsResponse' {httpStatus} -> httpStatus) (\s@ListMailDomainsResponse' {} a -> s {httpStatus = a} :: ListMailDomainsResponse)

instance Prelude.NFData ListMailDomainsResponse where
  rnf ListMailDomainsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf mailDomains
      `Prelude.seq` Prelude.rnf httpStatus
