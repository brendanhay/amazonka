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
-- Module      : Amazonka.FMS.ListAppsLists
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @AppsListDataSummary@ objects.
--
-- This operation returns paginated results.
module Amazonka.FMS.ListAppsLists
  ( -- * Creating a Request
    ListAppsLists (..),
    newListAppsLists,

    -- * Request Lenses
    listAppsLists_defaultLists,
    listAppsLists_nextToken,
    listAppsLists_maxResults,

    -- * Destructuring the Response
    ListAppsListsResponse (..),
    newListAppsListsResponse,

    -- * Response Lenses
    listAppsListsResponse_appsLists,
    listAppsListsResponse_nextToken,
    listAppsListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppsLists' smart constructor.
data ListAppsLists = ListAppsLists'
  { -- | Specifies whether the lists to retrieve are default lists owned by
    -- Firewall Manager.
    defaultLists :: Prelude.Maybe Prelude.Bool,
    -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, Firewall Manager returns this token
    -- in the response. For all but the first request, you provide the token
    -- returned by the prior request in the request parameters, to retrieve the
    -- next batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Firewall Manager to return
    -- for this request. If more objects are available, in the response,
    -- Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    --
    -- If you don\'t specify this, Firewall Manager returns all available
    -- objects.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppsLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultLists', 'listAppsLists_defaultLists' - Specifies whether the lists to retrieve are default lists owned by
-- Firewall Manager.
--
-- 'nextToken', 'listAppsLists_nextToken' - If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. For all but the first request, you provide the token
-- returned by the prior request in the request parameters, to retrieve the
-- next batch of objects.
--
-- 'maxResults', 'listAppsLists_maxResults' - The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- If you don\'t specify this, Firewall Manager returns all available
-- objects.
newListAppsLists ::
  -- | 'maxResults'
  Prelude.Natural ->
  ListAppsLists
newListAppsLists pMaxResults_ =
  ListAppsLists'
    { defaultLists = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = pMaxResults_
    }

-- | Specifies whether the lists to retrieve are default lists owned by
-- Firewall Manager.
listAppsLists_defaultLists :: Lens.Lens' ListAppsLists (Prelude.Maybe Prelude.Bool)
listAppsLists_defaultLists = Lens.lens (\ListAppsLists' {defaultLists} -> defaultLists) (\s@ListAppsLists' {} a -> s {defaultLists = a} :: ListAppsLists)

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. For all but the first request, you provide the token
-- returned by the prior request in the request parameters, to retrieve the
-- next batch of objects.
listAppsLists_nextToken :: Lens.Lens' ListAppsLists (Prelude.Maybe Prelude.Text)
listAppsLists_nextToken = Lens.lens (\ListAppsLists' {nextToken} -> nextToken) (\s@ListAppsLists' {} a -> s {nextToken = a} :: ListAppsLists)

-- | The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- If you don\'t specify this, Firewall Manager returns all available
-- objects.
listAppsLists_maxResults :: Lens.Lens' ListAppsLists Prelude.Natural
listAppsLists_maxResults = Lens.lens (\ListAppsLists' {maxResults} -> maxResults) (\s@ListAppsLists' {} a -> s {maxResults = a} :: ListAppsLists)

instance Core.AWSPager ListAppsLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppsListsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppsListsResponse_appsLists Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAppsLists_nextToken
          Lens..~ rs
          Lens.^? listAppsListsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAppsLists where
  type
    AWSResponse ListAppsLists =
      ListAppsListsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsListsResponse'
            Prelude.<$> (x Data..?> "AppsLists" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppsLists where
  hashWithSalt _salt ListAppsLists' {..} =
    _salt `Prelude.hashWithSalt` defaultLists
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAppsLists where
  rnf ListAppsLists' {..} =
    Prelude.rnf defaultLists
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListAppsLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.ListAppsLists" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppsLists where
  toJSON ListAppsLists' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultLists" Data..=) Prelude.<$> defaultLists,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("MaxResults" Data..= maxResults)
          ]
      )

instance Data.ToPath ListAppsLists where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAppsLists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppsListsResponse' smart constructor.
data ListAppsListsResponse = ListAppsListsResponse'
  { -- | An array of @AppsListDataSummary@ objects.
    appsLists :: Prelude.Maybe [AppsListDataSummary],
    -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, Firewall Manager returns this token
    -- in the response. You can use this token in subsequent requests to
    -- retrieve the next batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppsListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appsLists', 'listAppsListsResponse_appsLists' - An array of @AppsListDataSummary@ objects.
--
-- 'nextToken', 'listAppsListsResponse_nextToken' - If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. You can use this token in subsequent requests to
-- retrieve the next batch of objects.
--
-- 'httpStatus', 'listAppsListsResponse_httpStatus' - The response's http status code.
newListAppsListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppsListsResponse
newListAppsListsResponse pHttpStatus_ =
  ListAppsListsResponse'
    { appsLists = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @AppsListDataSummary@ objects.
listAppsListsResponse_appsLists :: Lens.Lens' ListAppsListsResponse (Prelude.Maybe [AppsListDataSummary])
listAppsListsResponse_appsLists = Lens.lens (\ListAppsListsResponse' {appsLists} -> appsLists) (\s@ListAppsListsResponse' {} a -> s {appsLists = a} :: ListAppsListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. You can use this token in subsequent requests to
-- retrieve the next batch of objects.
listAppsListsResponse_nextToken :: Lens.Lens' ListAppsListsResponse (Prelude.Maybe Prelude.Text)
listAppsListsResponse_nextToken = Lens.lens (\ListAppsListsResponse' {nextToken} -> nextToken) (\s@ListAppsListsResponse' {} a -> s {nextToken = a} :: ListAppsListsResponse)

-- | The response's http status code.
listAppsListsResponse_httpStatus :: Lens.Lens' ListAppsListsResponse Prelude.Int
listAppsListsResponse_httpStatus = Lens.lens (\ListAppsListsResponse' {httpStatus} -> httpStatus) (\s@ListAppsListsResponse' {} a -> s {httpStatus = a} :: ListAppsListsResponse)

instance Prelude.NFData ListAppsListsResponse where
  rnf ListAppsListsResponse' {..} =
    Prelude.rnf appsLists
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
