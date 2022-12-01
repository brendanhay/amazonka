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
-- Module      : Amazonka.SSMContacts.ListContacts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all contacts and escalation plans in Incident Manager.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListContacts
  ( -- * Creating a Request
    ListContacts (..),
    newListContacts,

    -- * Request Lenses
    listContacts_nextToken,
    listContacts_type,
    listContacts_maxResults,
    listContacts_aliasPrefix,

    -- * Destructuring the Response
    ListContactsResponse (..),
    newListContactsResponse,

    -- * Response Lenses
    listContactsResponse_nextToken,
    listContactsResponse_contacts,
    listContactsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListContacts' smart constructor.
data ListContacts = ListContacts'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of contact. A contact is type @PERSONAL@ and an escalation plan
    -- is type @ESCALATION@.
    type' :: Prelude.Maybe ContactType,
    -- | The maximum number of contacts and escalation plans per page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Used to list only contacts who\'s aliases start with the specified
    -- prefix.
    aliasPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContacts_nextToken' - The pagination token to continue to the next page of results.
--
-- 'type'', 'listContacts_type' - The type of contact. A contact is type @PERSONAL@ and an escalation plan
-- is type @ESCALATION@.
--
-- 'maxResults', 'listContacts_maxResults' - The maximum number of contacts and escalation plans per page of results.
--
-- 'aliasPrefix', 'listContacts_aliasPrefix' - Used to list only contacts who\'s aliases start with the specified
-- prefix.
newListContacts ::
  ListContacts
newListContacts =
  ListContacts'
    { nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      aliasPrefix = Prelude.Nothing
    }

-- | The pagination token to continue to the next page of results.
listContacts_nextToken :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Text)
listContacts_nextToken = Lens.lens (\ListContacts' {nextToken} -> nextToken) (\s@ListContacts' {} a -> s {nextToken = a} :: ListContacts)

-- | The type of contact. A contact is type @PERSONAL@ and an escalation plan
-- is type @ESCALATION@.
listContacts_type :: Lens.Lens' ListContacts (Prelude.Maybe ContactType)
listContacts_type = Lens.lens (\ListContacts' {type'} -> type') (\s@ListContacts' {} a -> s {type' = a} :: ListContacts)

-- | The maximum number of contacts and escalation plans per page of results.
listContacts_maxResults :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Natural)
listContacts_maxResults = Lens.lens (\ListContacts' {maxResults} -> maxResults) (\s@ListContacts' {} a -> s {maxResults = a} :: ListContacts)

-- | Used to list only contacts who\'s aliases start with the specified
-- prefix.
listContacts_aliasPrefix :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Text)
listContacts_aliasPrefix = Lens.lens (\ListContacts' {aliasPrefix} -> aliasPrefix) (\s@ListContacts' {} a -> s {aliasPrefix = a} :: ListContacts)

instance Core.AWSPager ListContacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listContactsResponse_contacts Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listContacts_nextToken
          Lens..~ rs
          Lens.^? listContactsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListContacts where
  type AWSResponse ListContacts = ListContactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Contacts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContacts where
  hashWithSalt _salt ListContacts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` aliasPrefix

instance Prelude.NFData ListContacts where
  rnf ListContacts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf aliasPrefix

instance Core.ToHeaders ListContacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SSMContacts.ListContacts" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListContacts where
  toJSON ListContacts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Type" Core..=) Prelude.<$> type',
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("AliasPrefix" Core..=) Prelude.<$> aliasPrefix
          ]
      )

instance Core.ToPath ListContacts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListContacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListContactsResponse' smart constructor.
data ListContactsResponse = ListContactsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the contacts and escalation plans in your Incident Manager
    -- account.
    contacts :: Prelude.Maybe [Contact],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'contacts', 'listContactsResponse_contacts' - A list of the contacts and escalation plans in your Incident Manager
-- account.
--
-- 'httpStatus', 'listContactsResponse_httpStatus' - The response's http status code.
newListContactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactsResponse
newListContactsResponse pHttpStatus_ =
  ListContactsResponse'
    { nextToken = Prelude.Nothing,
      contacts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to continue to the next page of results.
listContactsResponse_nextToken :: Lens.Lens' ListContactsResponse (Prelude.Maybe Prelude.Text)
listContactsResponse_nextToken = Lens.lens (\ListContactsResponse' {nextToken} -> nextToken) (\s@ListContactsResponse' {} a -> s {nextToken = a} :: ListContactsResponse)

-- | A list of the contacts and escalation plans in your Incident Manager
-- account.
listContactsResponse_contacts :: Lens.Lens' ListContactsResponse (Prelude.Maybe [Contact])
listContactsResponse_contacts = Lens.lens (\ListContactsResponse' {contacts} -> contacts) (\s@ListContactsResponse' {} a -> s {contacts = a} :: ListContactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listContactsResponse_httpStatus :: Lens.Lens' ListContactsResponse Prelude.Int
listContactsResponse_httpStatus = Lens.lens (\ListContactsResponse' {httpStatus} -> httpStatus) (\s@ListContactsResponse' {} a -> s {httpStatus = a} :: ListContactsResponse)

instance Prelude.NFData ListContactsResponse where
  rnf ListContactsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf contacts
      `Prelude.seq` Prelude.rnf httpStatus
