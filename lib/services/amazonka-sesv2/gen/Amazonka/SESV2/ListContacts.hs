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
-- Module      : Amazonka.SESV2.ListContacts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the contacts present in a specific contact list.
module Amazonka.SESV2.ListContacts
  ( -- * Creating a Request
    ListContacts (..),
    newListContacts,

    -- * Request Lenses
    listContacts_nextToken,
    listContacts_filter,
    listContacts_pageSize,
    listContacts_contactListName,

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | /See:/ 'newListContacts' smart constructor.
data ListContacts = ListContacts'
  { -- | A string token indicating that there might be additional contacts
    -- available to be listed. Use the token provided in the Response to use in
    -- the subsequent call to ListContacts with the same parameters to retrieve
    -- the next page of contacts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that can be applied to a list of contacts.
    filter' :: Prelude.Maybe ListContactsFilter,
    -- | The number of contacts that may be returned at once, which is dependent
    -- on if there are more or less contacts than the value of the PageSize.
    -- Use this parameter to paginate results. If additional contacts exist
    -- beyond the specified limit, the @NextToken@ element is sent in the
    -- response. Use the @NextToken@ value in subsequent requests to retrieve
    -- additional contacts.
    pageSize :: Prelude.Maybe Prelude.Int,
    -- | The name of the contact list.
    contactListName :: Prelude.Text
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
-- 'nextToken', 'listContacts_nextToken' - A string token indicating that there might be additional contacts
-- available to be listed. Use the token provided in the Response to use in
-- the subsequent call to ListContacts with the same parameters to retrieve
-- the next page of contacts.
--
-- 'filter'', 'listContacts_filter' - A filter that can be applied to a list of contacts.
--
-- 'pageSize', 'listContacts_pageSize' - The number of contacts that may be returned at once, which is dependent
-- on if there are more or less contacts than the value of the PageSize.
-- Use this parameter to paginate results. If additional contacts exist
-- beyond the specified limit, the @NextToken@ element is sent in the
-- response. Use the @NextToken@ value in subsequent requests to retrieve
-- additional contacts.
--
-- 'contactListName', 'listContacts_contactListName' - The name of the contact list.
newListContacts ::
  -- | 'contactListName'
  Prelude.Text ->
  ListContacts
newListContacts pContactListName_ =
  ListContacts'
    { nextToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      contactListName = pContactListName_
    }

-- | A string token indicating that there might be additional contacts
-- available to be listed. Use the token provided in the Response to use in
-- the subsequent call to ListContacts with the same parameters to retrieve
-- the next page of contacts.
listContacts_nextToken :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Text)
listContacts_nextToken = Lens.lens (\ListContacts' {nextToken} -> nextToken) (\s@ListContacts' {} a -> s {nextToken = a} :: ListContacts)

-- | A filter that can be applied to a list of contacts.
listContacts_filter :: Lens.Lens' ListContacts (Prelude.Maybe ListContactsFilter)
listContacts_filter = Lens.lens (\ListContacts' {filter'} -> filter') (\s@ListContacts' {} a -> s {filter' = a} :: ListContacts)

-- | The number of contacts that may be returned at once, which is dependent
-- on if there are more or less contacts than the value of the PageSize.
-- Use this parameter to paginate results. If additional contacts exist
-- beyond the specified limit, the @NextToken@ element is sent in the
-- response. Use the @NextToken@ value in subsequent requests to retrieve
-- additional contacts.
listContacts_pageSize :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Int)
listContacts_pageSize = Lens.lens (\ListContacts' {pageSize} -> pageSize) (\s@ListContacts' {} a -> s {pageSize = a} :: ListContacts)

-- | The name of the contact list.
listContacts_contactListName :: Lens.Lens' ListContacts Prelude.Text
listContacts_contactListName = Lens.lens (\ListContacts' {contactListName} -> contactListName) (\s@ListContacts' {} a -> s {contactListName = a} :: ListContacts)

instance Core.AWSRequest ListContacts where
  type AWSResponse ListContacts = ListContactsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Contacts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContacts where
  hashWithSalt _salt ListContacts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` contactListName

instance Prelude.NFData ListContacts where
  rnf ListContacts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf contactListName

instance Data.ToHeaders ListContacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListContacts where
  toPath ListContacts' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Data.toBS contactListName,
        "/contacts"
      ]

instance Data.ToQuery ListContacts where
  toQuery ListContacts' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "PageSize" Data.=: pageSize
      ]

-- | /See:/ 'newListContactsResponse' smart constructor.
data ListContactsResponse = ListContactsResponse'
  { -- | A string token indicating that there might be additional contacts
    -- available to be listed. Copy this token to a subsequent call to
    -- @ListContacts@ with the same parameters to retrieve the next page of
    -- contacts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The contacts present in a specific contact list.
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
-- 'nextToken', 'listContactsResponse_nextToken' - A string token indicating that there might be additional contacts
-- available to be listed. Copy this token to a subsequent call to
-- @ListContacts@ with the same parameters to retrieve the next page of
-- contacts.
--
-- 'contacts', 'listContactsResponse_contacts' - The contacts present in a specific contact list.
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

-- | A string token indicating that there might be additional contacts
-- available to be listed. Copy this token to a subsequent call to
-- @ListContacts@ with the same parameters to retrieve the next page of
-- contacts.
listContactsResponse_nextToken :: Lens.Lens' ListContactsResponse (Prelude.Maybe Prelude.Text)
listContactsResponse_nextToken = Lens.lens (\ListContactsResponse' {nextToken} -> nextToken) (\s@ListContactsResponse' {} a -> s {nextToken = a} :: ListContactsResponse)

-- | The contacts present in a specific contact list.
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
