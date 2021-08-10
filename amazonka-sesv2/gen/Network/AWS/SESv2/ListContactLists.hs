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
-- Module      : Network.AWS.SESv2.ListContactLists
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the contact lists available.
module Network.AWS.SESv2.ListContactLists
  ( -- * Creating a Request
    ListContactLists (..),
    newListContactLists,

    -- * Request Lenses
    listContactLists_nextToken,
    listContactLists_pageSize,

    -- * Destructuring the Response
    ListContactListsResponse (..),
    newListContactListsResponse,

    -- * Response Lenses
    listContactListsResponse_nextToken,
    listContactListsResponse_contactLists,
    listContactListsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | /See:/ 'newListContactLists' smart constructor.
data ListContactLists = ListContactLists'
  { -- | A string token indicating that there might be additional contact lists
    -- available to be listed. Use the token provided in the Response to use in
    -- the subsequent call to ListContactLists with the same parameters to
    -- retrieve the next page of contact lists.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of contact lists to return at once. Use this parameter to
    -- paginate results. If additional contact lists exist beyond the specified
    -- limit, the @NextToken@ element is sent in the response. Use the
    -- @NextToken@ value in subsequent requests to retrieve additional lists.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactLists_nextToken' - A string token indicating that there might be additional contact lists
-- available to be listed. Use the token provided in the Response to use in
-- the subsequent call to ListContactLists with the same parameters to
-- retrieve the next page of contact lists.
--
-- 'pageSize', 'listContactLists_pageSize' - Maximum number of contact lists to return at once. Use this parameter to
-- paginate results. If additional contact lists exist beyond the specified
-- limit, the @NextToken@ element is sent in the response. Use the
-- @NextToken@ value in subsequent requests to retrieve additional lists.
newListContactLists ::
  ListContactLists
newListContactLists =
  ListContactLists'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A string token indicating that there might be additional contact lists
-- available to be listed. Use the token provided in the Response to use in
-- the subsequent call to ListContactLists with the same parameters to
-- retrieve the next page of contact lists.
listContactLists_nextToken :: Lens.Lens' ListContactLists (Prelude.Maybe Prelude.Text)
listContactLists_nextToken = Lens.lens (\ListContactLists' {nextToken} -> nextToken) (\s@ListContactLists' {} a -> s {nextToken = a} :: ListContactLists)

-- | Maximum number of contact lists to return at once. Use this parameter to
-- paginate results. If additional contact lists exist beyond the specified
-- limit, the @NextToken@ element is sent in the response. Use the
-- @NextToken@ value in subsequent requests to retrieve additional lists.
listContactLists_pageSize :: Lens.Lens' ListContactLists (Prelude.Maybe Prelude.Int)
listContactLists_pageSize = Lens.lens (\ListContactLists' {pageSize} -> pageSize) (\s@ListContactLists' {} a -> s {pageSize = a} :: ListContactLists)

instance Core.AWSRequest ListContactLists where
  type
    AWSResponse ListContactLists =
      ListContactListsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactListsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ContactLists" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContactLists

instance Prelude.NFData ListContactLists

instance Core.ToHeaders ListContactLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListContactLists where
  toPath = Prelude.const "/v2/email/contact-lists"

instance Core.ToQuery ListContactLists where
  toQuery ListContactLists' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "PageSize" Core.=: pageSize
      ]

-- | /See:/ 'newListContactListsResponse' smart constructor.
data ListContactListsResponse = ListContactListsResponse'
  { -- | A string token indicating that there might be additional contact lists
    -- available to be listed. Copy this token to a subsequent call to
    -- @ListContactLists@ with the same parameters to retrieve the next page of
    -- contact lists.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The available contact lists.
    contactLists :: Prelude.Maybe [ContactList],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactListsResponse_nextToken' - A string token indicating that there might be additional contact lists
-- available to be listed. Copy this token to a subsequent call to
-- @ListContactLists@ with the same parameters to retrieve the next page of
-- contact lists.
--
-- 'contactLists', 'listContactListsResponse_contactLists' - The available contact lists.
--
-- 'httpStatus', 'listContactListsResponse_httpStatus' - The response's http status code.
newListContactListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactListsResponse
newListContactListsResponse pHttpStatus_ =
  ListContactListsResponse'
    { nextToken =
        Prelude.Nothing,
      contactLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string token indicating that there might be additional contact lists
-- available to be listed. Copy this token to a subsequent call to
-- @ListContactLists@ with the same parameters to retrieve the next page of
-- contact lists.
listContactListsResponse_nextToken :: Lens.Lens' ListContactListsResponse (Prelude.Maybe Prelude.Text)
listContactListsResponse_nextToken = Lens.lens (\ListContactListsResponse' {nextToken} -> nextToken) (\s@ListContactListsResponse' {} a -> s {nextToken = a} :: ListContactListsResponse)

-- | The available contact lists.
listContactListsResponse_contactLists :: Lens.Lens' ListContactListsResponse (Prelude.Maybe [ContactList])
listContactListsResponse_contactLists = Lens.lens (\ListContactListsResponse' {contactLists} -> contactLists) (\s@ListContactListsResponse' {} a -> s {contactLists = a} :: ListContactListsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listContactListsResponse_httpStatus :: Lens.Lens' ListContactListsResponse Prelude.Int
listContactListsResponse_httpStatus = Lens.lens (\ListContactListsResponse' {httpStatus} -> httpStatus) (\s@ListContactListsResponse' {} a -> s {httpStatus = a} :: ListContactListsResponse)

instance Prelude.NFData ListContactListsResponse
