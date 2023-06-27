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
-- Module      : Amazonka.SSMContacts.ListPageResolutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resolution path of an engagement. For example, the
-- escalation plan engaged in an incident might target an on-call schedule
-- that includes several contacts in a rotation, but just one contact
-- on-call when the incident starts. The resolution path indicates the
-- hierarchy of /escalation plan > on-call schedule > contact/.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListPageResolutions
  ( -- * Creating a Request
    ListPageResolutions (..),
    newListPageResolutions,

    -- * Request Lenses
    listPageResolutions_nextToken,
    listPageResolutions_pageId,

    -- * Destructuring the Response
    ListPageResolutionsResponse (..),
    newListPageResolutionsResponse,

    -- * Response Lenses
    listPageResolutionsResponse_nextToken,
    listPageResolutionsResponse_httpStatus,
    listPageResolutionsResponse_pageResolutions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListPageResolutions' smart constructor.
data ListPageResolutions = ListPageResolutions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the contact engaged for the incident.
    pageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPageResolutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPageResolutions_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'pageId', 'listPageResolutions_pageId' - The Amazon Resource Name (ARN) of the contact engaged for the incident.
newListPageResolutions ::
  -- | 'pageId'
  Prelude.Text ->
  ListPageResolutions
newListPageResolutions pPageId_ =
  ListPageResolutions'
    { nextToken = Prelude.Nothing,
      pageId = pPageId_
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listPageResolutions_nextToken :: Lens.Lens' ListPageResolutions (Prelude.Maybe Prelude.Text)
listPageResolutions_nextToken = Lens.lens (\ListPageResolutions' {nextToken} -> nextToken) (\s@ListPageResolutions' {} a -> s {nextToken = a} :: ListPageResolutions)

-- | The Amazon Resource Name (ARN) of the contact engaged for the incident.
listPageResolutions_pageId :: Lens.Lens' ListPageResolutions Prelude.Text
listPageResolutions_pageId = Lens.lens (\ListPageResolutions' {pageId} -> pageId) (\s@ListPageResolutions' {} a -> s {pageId = a} :: ListPageResolutions)

instance Core.AWSPager ListPageResolutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPageResolutionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listPageResolutionsResponse_pageResolutions
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPageResolutions_nextToken
          Lens..~ rs
          Lens.^? listPageResolutionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPageResolutions where
  type
    AWSResponse ListPageResolutions =
      ListPageResolutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPageResolutionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "PageResolutions"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListPageResolutions where
  hashWithSalt _salt ListPageResolutions' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageId

instance Prelude.NFData ListPageResolutions where
  rnf ListPageResolutions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageId

instance Data.ToHeaders ListPageResolutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.ListPageResolutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPageResolutions where
  toJSON ListPageResolutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("PageId" Data..= pageId)
          ]
      )

instance Data.ToPath ListPageResolutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPageResolutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPageResolutionsResponse' smart constructor.
data ListPageResolutionsResponse = ListPageResolutionsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the resolution for an engagement.
    pageResolutions :: [ResolutionContact]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPageResolutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPageResolutionsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listPageResolutionsResponse_httpStatus' - The response's http status code.
--
-- 'pageResolutions', 'listPageResolutionsResponse_pageResolutions' - Information about the resolution for an engagement.
newListPageResolutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPageResolutionsResponse
newListPageResolutionsResponse pHttpStatus_ =
  ListPageResolutionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      pageResolutions = Prelude.mempty
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listPageResolutionsResponse_nextToken :: Lens.Lens' ListPageResolutionsResponse (Prelude.Maybe Prelude.Text)
listPageResolutionsResponse_nextToken = Lens.lens (\ListPageResolutionsResponse' {nextToken} -> nextToken) (\s@ListPageResolutionsResponse' {} a -> s {nextToken = a} :: ListPageResolutionsResponse)

-- | The response's http status code.
listPageResolutionsResponse_httpStatus :: Lens.Lens' ListPageResolutionsResponse Prelude.Int
listPageResolutionsResponse_httpStatus = Lens.lens (\ListPageResolutionsResponse' {httpStatus} -> httpStatus) (\s@ListPageResolutionsResponse' {} a -> s {httpStatus = a} :: ListPageResolutionsResponse)

-- | Information about the resolution for an engagement.
listPageResolutionsResponse_pageResolutions :: Lens.Lens' ListPageResolutionsResponse [ResolutionContact]
listPageResolutionsResponse_pageResolutions = Lens.lens (\ListPageResolutionsResponse' {pageResolutions} -> pageResolutions) (\s@ListPageResolutionsResponse' {} a -> s {pageResolutions = a} :: ListPageResolutionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPageResolutionsResponse where
  rnf ListPageResolutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pageResolutions
