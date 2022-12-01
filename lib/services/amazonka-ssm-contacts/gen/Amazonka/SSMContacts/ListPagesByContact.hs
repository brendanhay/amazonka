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
-- Module      : Amazonka.SSMContacts.ListPagesByContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the engagements to a contact\'s contact channels.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListPagesByContact
  ( -- * Creating a Request
    ListPagesByContact (..),
    newListPagesByContact,

    -- * Request Lenses
    listPagesByContact_nextToken,
    listPagesByContact_maxResults,
    listPagesByContact_contactId,

    -- * Destructuring the Response
    ListPagesByContactResponse (..),
    newListPagesByContactResponse,

    -- * Response Lenses
    listPagesByContactResponse_nextToken,
    listPagesByContactResponse_httpStatus,
    listPagesByContactResponse_pages,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListPagesByContact' smart constructor.
data ListPagesByContact = ListPagesByContact'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of engagements to contact channels to list per page
    -- of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the contact you are retrieving
    -- engagements for.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPagesByContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPagesByContact_nextToken' - The pagination token to continue to the next page of results.
--
-- 'maxResults', 'listPagesByContact_maxResults' - The maximum number of engagements to contact channels to list per page
-- of results.
--
-- 'contactId', 'listPagesByContact_contactId' - The Amazon Resource Name (ARN) of the contact you are retrieving
-- engagements for.
newListPagesByContact ::
  -- | 'contactId'
  Prelude.Text ->
  ListPagesByContact
newListPagesByContact pContactId_ =
  ListPagesByContact'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      contactId = pContactId_
    }

-- | The pagination token to continue to the next page of results.
listPagesByContact_nextToken :: Lens.Lens' ListPagesByContact (Prelude.Maybe Prelude.Text)
listPagesByContact_nextToken = Lens.lens (\ListPagesByContact' {nextToken} -> nextToken) (\s@ListPagesByContact' {} a -> s {nextToken = a} :: ListPagesByContact)

-- | The maximum number of engagements to contact channels to list per page
-- of results.
listPagesByContact_maxResults :: Lens.Lens' ListPagesByContact (Prelude.Maybe Prelude.Natural)
listPagesByContact_maxResults = Lens.lens (\ListPagesByContact' {maxResults} -> maxResults) (\s@ListPagesByContact' {} a -> s {maxResults = a} :: ListPagesByContact)

-- | The Amazon Resource Name (ARN) of the contact you are retrieving
-- engagements for.
listPagesByContact_contactId :: Lens.Lens' ListPagesByContact Prelude.Text
listPagesByContact_contactId = Lens.lens (\ListPagesByContact' {contactId} -> contactId) (\s@ListPagesByContact' {} a -> s {contactId = a} :: ListPagesByContact)

instance Core.AWSPager ListPagesByContact where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPagesByContactResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listPagesByContactResponse_pages) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPagesByContact_nextToken
          Lens..~ rs
          Lens.^? listPagesByContactResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPagesByContact where
  type
    AWSResponse ListPagesByContact =
      ListPagesByContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPagesByContactResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Pages" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPagesByContact where
  hashWithSalt _salt ListPagesByContact' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` contactId

instance Prelude.NFData ListPagesByContact where
  rnf ListPagesByContact' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf contactId

instance Core.ToHeaders ListPagesByContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SSMContacts.ListPagesByContact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPagesByContact where
  toJSON ListPagesByContact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ContactId" Core..= contactId)
          ]
      )

instance Core.ToPath ListPagesByContact where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPagesByContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPagesByContactResponse' smart constructor.
data ListPagesByContactResponse = ListPagesByContactResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of engagements to a contact\'s contact channel.
    pages :: [Page]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPagesByContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPagesByContactResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listPagesByContactResponse_httpStatus' - The response's http status code.
--
-- 'pages', 'listPagesByContactResponse_pages' - The list of engagements to a contact\'s contact channel.
newListPagesByContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPagesByContactResponse
newListPagesByContactResponse pHttpStatus_ =
  ListPagesByContactResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      pages = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listPagesByContactResponse_nextToken :: Lens.Lens' ListPagesByContactResponse (Prelude.Maybe Prelude.Text)
listPagesByContactResponse_nextToken = Lens.lens (\ListPagesByContactResponse' {nextToken} -> nextToken) (\s@ListPagesByContactResponse' {} a -> s {nextToken = a} :: ListPagesByContactResponse)

-- | The response's http status code.
listPagesByContactResponse_httpStatus :: Lens.Lens' ListPagesByContactResponse Prelude.Int
listPagesByContactResponse_httpStatus = Lens.lens (\ListPagesByContactResponse' {httpStatus} -> httpStatus) (\s@ListPagesByContactResponse' {} a -> s {httpStatus = a} :: ListPagesByContactResponse)

-- | The list of engagements to a contact\'s contact channel.
listPagesByContactResponse_pages :: Lens.Lens' ListPagesByContactResponse [Page]
listPagesByContactResponse_pages = Lens.lens (\ListPagesByContactResponse' {pages} -> pages) (\s@ListPagesByContactResponse' {} a -> s {pages = a} :: ListPagesByContactResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPagesByContactResponse where
  rnf ListPagesByContactResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pages
