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
-- Module      : Amazonka.ConnectCases.ListCasesForContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists cases for a given contact.
module Amazonka.ConnectCases.ListCasesForContact
  ( -- * Creating a Request
    ListCasesForContact (..),
    newListCasesForContact,

    -- * Request Lenses
    listCasesForContact_maxResults,
    listCasesForContact_nextToken,
    listCasesForContact_contactArn,
    listCasesForContact_domainId,

    -- * Destructuring the Response
    ListCasesForContactResponse (..),
    newListCasesForContactResponse,

    -- * Response Lenses
    listCasesForContactResponse_nextToken,
    listCasesForContactResponse_httpStatus,
    listCasesForContactResponse_cases,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCasesForContact' smart constructor.
data ListCasesForContact = ListCasesForContact'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier of a contact in Amazon Connect.
    contactArn :: Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCasesForContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCasesForContact_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listCasesForContact_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'contactArn', 'listCasesForContact_contactArn' - A unique identifier of a contact in Amazon Connect.
--
-- 'domainId', 'listCasesForContact_domainId' - The unique identifier of the Cases domain.
newListCasesForContact ::
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  ListCasesForContact
newListCasesForContact pContactArn_ pDomainId_ =
  ListCasesForContact'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      contactArn = pContactArn_,
      domainId = pDomainId_
    }

-- | The maximum number of results to return per page.
listCasesForContact_maxResults :: Lens.Lens' ListCasesForContact (Prelude.Maybe Prelude.Natural)
listCasesForContact_maxResults = Lens.lens (\ListCasesForContact' {maxResults} -> maxResults) (\s@ListCasesForContact' {} a -> s {maxResults = a} :: ListCasesForContact)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listCasesForContact_nextToken :: Lens.Lens' ListCasesForContact (Prelude.Maybe Prelude.Text)
listCasesForContact_nextToken = Lens.lens (\ListCasesForContact' {nextToken} -> nextToken) (\s@ListCasesForContact' {} a -> s {nextToken = a} :: ListCasesForContact)

-- | A unique identifier of a contact in Amazon Connect.
listCasesForContact_contactArn :: Lens.Lens' ListCasesForContact Prelude.Text
listCasesForContact_contactArn = Lens.lens (\ListCasesForContact' {contactArn} -> contactArn) (\s@ListCasesForContact' {} a -> s {contactArn = a} :: ListCasesForContact)

-- | The unique identifier of the Cases domain.
listCasesForContact_domainId :: Lens.Lens' ListCasesForContact Prelude.Text
listCasesForContact_domainId = Lens.lens (\ListCasesForContact' {domainId} -> domainId) (\s@ListCasesForContact' {} a -> s {domainId = a} :: ListCasesForContact)

instance Core.AWSRequest ListCasesForContact where
  type
    AWSResponse ListCasesForContact =
      ListCasesForContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCasesForContactResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "cases" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListCasesForContact where
  hashWithSalt _salt ListCasesForContact' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListCasesForContact where
  rnf ListCasesForContact' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf contactArn `Prelude.seq`
          Prelude.rnf domainId

instance Data.ToHeaders ListCasesForContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCasesForContact where
  toJSON ListCasesForContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("contactArn" Data..= contactArn)
          ]
      )

instance Data.ToPath ListCasesForContact where
  toPath ListCasesForContact' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/list-cases-for-contact"
      ]

instance Data.ToQuery ListCasesForContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCasesForContactResponse' smart constructor.
data ListCasesForContactResponse = ListCasesForContactResponse'
  { -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of Case summary information.
    cases :: [CaseSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCasesForContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCasesForContactResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'listCasesForContactResponse_httpStatus' - The response's http status code.
--
-- 'cases', 'listCasesForContactResponse_cases' - A list of Case summary information.
newListCasesForContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCasesForContactResponse
newListCasesForContactResponse pHttpStatus_ =
  ListCasesForContactResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      cases = Prelude.mempty
    }

-- | The token for the next set of results. This is null if there are no more
-- results to return.
listCasesForContactResponse_nextToken :: Lens.Lens' ListCasesForContactResponse (Prelude.Maybe Prelude.Text)
listCasesForContactResponse_nextToken = Lens.lens (\ListCasesForContactResponse' {nextToken} -> nextToken) (\s@ListCasesForContactResponse' {} a -> s {nextToken = a} :: ListCasesForContactResponse)

-- | The response's http status code.
listCasesForContactResponse_httpStatus :: Lens.Lens' ListCasesForContactResponse Prelude.Int
listCasesForContactResponse_httpStatus = Lens.lens (\ListCasesForContactResponse' {httpStatus} -> httpStatus) (\s@ListCasesForContactResponse' {} a -> s {httpStatus = a} :: ListCasesForContactResponse)

-- | A list of Case summary information.
listCasesForContactResponse_cases :: Lens.Lens' ListCasesForContactResponse [CaseSummary]
listCasesForContactResponse_cases = Lens.lens (\ListCasesForContactResponse' {cases} -> cases) (\s@ListCasesForContactResponse' {} a -> s {cases = a} :: ListCasesForContactResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListCasesForContactResponse where
  rnf ListCasesForContactResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf cases
