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
-- Module      : Amazonka.SSMContacts.ListContactChannels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all contact channels for the specified contact.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListContactChannels
  ( -- * Creating a Request
    ListContactChannels (..),
    newListContactChannels,

    -- * Request Lenses
    listContactChannels_maxResults,
    listContactChannels_nextToken,
    listContactChannels_contactId,

    -- * Destructuring the Response
    ListContactChannelsResponse (..),
    newListContactChannelsResponse,

    -- * Response Lenses
    listContactChannelsResponse_nextToken,
    listContactChannelsResponse_httpStatus,
    listContactChannelsResponse_contactChannels,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListContactChannels' smart constructor.
data ListContactChannels = ListContactChannels'
  { -- | The maximum number of contact channels per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the contact.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listContactChannels_maxResults' - The maximum number of contact channels per page.
--
-- 'nextToken', 'listContactChannels_nextToken' - The pagination token to continue to the next page of results.
--
-- 'contactId', 'listContactChannels_contactId' - The Amazon Resource Name (ARN) of the contact.
newListContactChannels ::
  -- | 'contactId'
  Prelude.Text ->
  ListContactChannels
newListContactChannels pContactId_ =
  ListContactChannels'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      contactId = pContactId_
    }

-- | The maximum number of contact channels per page.
listContactChannels_maxResults :: Lens.Lens' ListContactChannels (Prelude.Maybe Prelude.Natural)
listContactChannels_maxResults = Lens.lens (\ListContactChannels' {maxResults} -> maxResults) (\s@ListContactChannels' {} a -> s {maxResults = a} :: ListContactChannels)

-- | The pagination token to continue to the next page of results.
listContactChannels_nextToken :: Lens.Lens' ListContactChannels (Prelude.Maybe Prelude.Text)
listContactChannels_nextToken = Lens.lens (\ListContactChannels' {nextToken} -> nextToken) (\s@ListContactChannels' {} a -> s {nextToken = a} :: ListContactChannels)

-- | The Amazon Resource Name (ARN) of the contact.
listContactChannels_contactId :: Lens.Lens' ListContactChannels Prelude.Text
listContactChannels_contactId = Lens.lens (\ListContactChannels' {contactId} -> contactId) (\s@ListContactChannels' {} a -> s {contactId = a} :: ListContactChannels)

instance Core.AWSPager ListContactChannels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactChannelsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listContactChannelsResponse_contactChannels
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listContactChannels_nextToken
          Lens..~ rs
          Lens.^? listContactChannelsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListContactChannels where
  type
    AWSResponse ListContactChannels =
      ListContactChannelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactChannelsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ContactChannels"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListContactChannels where
  hashWithSalt _salt ListContactChannels' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` contactId

instance Prelude.NFData ListContactChannels where
  rnf ListContactChannels' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf contactId

instance Data.ToHeaders ListContactChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.ListContactChannels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListContactChannels where
  toJSON ListContactChannels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ContactId" Data..= contactId)
          ]
      )

instance Data.ToPath ListContactChannels where
  toPath = Prelude.const "/"

instance Data.ToQuery ListContactChannels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListContactChannelsResponse' smart constructor.
data ListContactChannelsResponse = ListContactChannelsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of contact channels related to the specified contact.
    contactChannels :: [ContactChannel]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactChannelsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listContactChannelsResponse_httpStatus' - The response's http status code.
--
-- 'contactChannels', 'listContactChannelsResponse_contactChannels' - A list of contact channels related to the specified contact.
newListContactChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactChannelsResponse
newListContactChannelsResponse pHttpStatus_ =
  ListContactChannelsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      contactChannels = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listContactChannelsResponse_nextToken :: Lens.Lens' ListContactChannelsResponse (Prelude.Maybe Prelude.Text)
listContactChannelsResponse_nextToken = Lens.lens (\ListContactChannelsResponse' {nextToken} -> nextToken) (\s@ListContactChannelsResponse' {} a -> s {nextToken = a} :: ListContactChannelsResponse)

-- | The response's http status code.
listContactChannelsResponse_httpStatus :: Lens.Lens' ListContactChannelsResponse Prelude.Int
listContactChannelsResponse_httpStatus = Lens.lens (\ListContactChannelsResponse' {httpStatus} -> httpStatus) (\s@ListContactChannelsResponse' {} a -> s {httpStatus = a} :: ListContactChannelsResponse)

-- | A list of contact channels related to the specified contact.
listContactChannelsResponse_contactChannels :: Lens.Lens' ListContactChannelsResponse [ContactChannel]
listContactChannelsResponse_contactChannels = Lens.lens (\ListContactChannelsResponse' {contactChannels} -> contactChannels) (\s@ListContactChannelsResponse' {} a -> s {contactChannels = a} :: ListContactChannelsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListContactChannelsResponse where
  rnf ListContactChannelsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contactChannels
