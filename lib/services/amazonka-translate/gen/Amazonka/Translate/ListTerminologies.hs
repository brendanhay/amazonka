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
-- Module      : Amazonka.Translate.ListTerminologies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom terminologies associated with your account.
--
-- This operation returns paginated results.
module Amazonka.Translate.ListTerminologies
  ( -- * Creating a Request
    ListTerminologies (..),
    newListTerminologies,

    -- * Request Lenses
    listTerminologies_nextToken,
    listTerminologies_maxResults,

    -- * Destructuring the Response
    ListTerminologiesResponse (..),
    newListTerminologiesResponse,

    -- * Response Lenses
    listTerminologiesResponse_nextToken,
    listTerminologiesResponse_terminologyPropertiesList,
    listTerminologiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newListTerminologies' smart constructor.
data ListTerminologies = ListTerminologies'
  { -- | If the result of the request to ListTerminologies was truncated, include
    -- the NextToken to fetch the next group of custom terminologies.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of custom terminologies returned per list request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTerminologies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTerminologies_nextToken' - If the result of the request to ListTerminologies was truncated, include
-- the NextToken to fetch the next group of custom terminologies.
--
-- 'maxResults', 'listTerminologies_maxResults' - The maximum number of custom terminologies returned per list request.
newListTerminologies ::
  ListTerminologies
newListTerminologies =
  ListTerminologies'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the request to ListTerminologies was truncated, include
-- the NextToken to fetch the next group of custom terminologies.
listTerminologies_nextToken :: Lens.Lens' ListTerminologies (Prelude.Maybe Prelude.Text)
listTerminologies_nextToken = Lens.lens (\ListTerminologies' {nextToken} -> nextToken) (\s@ListTerminologies' {} a -> s {nextToken = a} :: ListTerminologies)

-- | The maximum number of custom terminologies returned per list request.
listTerminologies_maxResults :: Lens.Lens' ListTerminologies (Prelude.Maybe Prelude.Natural)
listTerminologies_maxResults = Lens.lens (\ListTerminologies' {maxResults} -> maxResults) (\s@ListTerminologies' {} a -> s {maxResults = a} :: ListTerminologies)

instance Core.AWSPager ListTerminologies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTerminologiesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTerminologiesResponse_terminologyPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTerminologies_nextToken
          Lens..~ rs
          Lens.^? listTerminologiesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTerminologies where
  type
    AWSResponse ListTerminologies =
      ListTerminologiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTerminologiesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "TerminologyPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTerminologies where
  hashWithSalt _salt ListTerminologies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListTerminologies where
  rnf ListTerminologies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListTerminologies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.ListTerminologies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTerminologies where
  toJSON ListTerminologies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListTerminologies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTerminologies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTerminologiesResponse' smart constructor.
data ListTerminologiesResponse = ListTerminologiesResponse'
  { -- | If the response to the ListTerminologies was truncated, the NextToken
    -- fetches the next group of custom terminologies.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The properties list of the custom terminologies returned on the list
    -- request.
    terminologyPropertiesList :: Prelude.Maybe [TerminologyProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTerminologiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTerminologiesResponse_nextToken' - If the response to the ListTerminologies was truncated, the NextToken
-- fetches the next group of custom terminologies.
--
-- 'terminologyPropertiesList', 'listTerminologiesResponse_terminologyPropertiesList' - The properties list of the custom terminologies returned on the list
-- request.
--
-- 'httpStatus', 'listTerminologiesResponse_httpStatus' - The response's http status code.
newListTerminologiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTerminologiesResponse
newListTerminologiesResponse pHttpStatus_ =
  ListTerminologiesResponse'
    { nextToken =
        Prelude.Nothing,
      terminologyPropertiesList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response to the ListTerminologies was truncated, the NextToken
-- fetches the next group of custom terminologies.
listTerminologiesResponse_nextToken :: Lens.Lens' ListTerminologiesResponse (Prelude.Maybe Prelude.Text)
listTerminologiesResponse_nextToken = Lens.lens (\ListTerminologiesResponse' {nextToken} -> nextToken) (\s@ListTerminologiesResponse' {} a -> s {nextToken = a} :: ListTerminologiesResponse)

-- | The properties list of the custom terminologies returned on the list
-- request.
listTerminologiesResponse_terminologyPropertiesList :: Lens.Lens' ListTerminologiesResponse (Prelude.Maybe [TerminologyProperties])
listTerminologiesResponse_terminologyPropertiesList = Lens.lens (\ListTerminologiesResponse' {terminologyPropertiesList} -> terminologyPropertiesList) (\s@ListTerminologiesResponse' {} a -> s {terminologyPropertiesList = a} :: ListTerminologiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTerminologiesResponse_httpStatus :: Lens.Lens' ListTerminologiesResponse Prelude.Int
listTerminologiesResponse_httpStatus = Lens.lens (\ListTerminologiesResponse' {httpStatus} -> httpStatus) (\s@ListTerminologiesResponse' {} a -> s {httpStatus = a} :: ListTerminologiesResponse)

instance Prelude.NFData ListTerminologiesResponse where
  rnf ListTerminologiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf terminologyPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
