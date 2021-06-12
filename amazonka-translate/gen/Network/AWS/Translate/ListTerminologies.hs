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
-- Module      : Network.AWS.Translate.ListTerminologies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom terminologies associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.Translate.ListTerminologies
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newListTerminologies' smart constructor.
data ListTerminologies = ListTerminologies'
  { -- | If the result of the request to ListTerminologies was truncated, include
    -- the NextToken to fetch the next group of custom terminologies.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of custom terminologies returned per list request.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If the result of the request to ListTerminologies was truncated, include
-- the NextToken to fetch the next group of custom terminologies.
listTerminologies_nextToken :: Lens.Lens' ListTerminologies (Core.Maybe Core.Text)
listTerminologies_nextToken = Lens.lens (\ListTerminologies' {nextToken} -> nextToken) (\s@ListTerminologies' {} a -> s {nextToken = a} :: ListTerminologies)

-- | The maximum number of custom terminologies returned per list request.
listTerminologies_maxResults :: Lens.Lens' ListTerminologies (Core.Maybe Core.Natural)
listTerminologies_maxResults = Lens.lens (\ListTerminologies' {maxResults} -> maxResults) (\s@ListTerminologies' {} a -> s {maxResults = a} :: ListTerminologies)

instance Core.AWSPager ListTerminologies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTerminologiesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTerminologiesResponse_terminologyPropertiesList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTerminologies_nextToken
          Lens..~ rs
          Lens.^? listTerminologiesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListTerminologies where
  type
    AWSResponse ListTerminologies =
      ListTerminologiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTerminologiesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "TerminologyPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTerminologies

instance Core.NFData ListTerminologies

instance Core.ToHeaders ListTerminologies where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.ListTerminologies" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTerminologies where
  toJSON ListTerminologies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListTerminologies where
  toPath = Core.const "/"

instance Core.ToQuery ListTerminologies where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTerminologiesResponse' smart constructor.
data ListTerminologiesResponse = ListTerminologiesResponse'
  { -- | If the response to the ListTerminologies was truncated, the NextToken
    -- fetches the next group of custom terminologies.
    nextToken :: Core.Maybe Core.Text,
    -- | The properties list of the custom terminologies returned on the list
    -- request.
    terminologyPropertiesList :: Core.Maybe [TerminologyProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListTerminologiesResponse
newListTerminologiesResponse pHttpStatus_ =
  ListTerminologiesResponse'
    { nextToken =
        Core.Nothing,
      terminologyPropertiesList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response to the ListTerminologies was truncated, the NextToken
-- fetches the next group of custom terminologies.
listTerminologiesResponse_nextToken :: Lens.Lens' ListTerminologiesResponse (Core.Maybe Core.Text)
listTerminologiesResponse_nextToken = Lens.lens (\ListTerminologiesResponse' {nextToken} -> nextToken) (\s@ListTerminologiesResponse' {} a -> s {nextToken = a} :: ListTerminologiesResponse)

-- | The properties list of the custom terminologies returned on the list
-- request.
listTerminologiesResponse_terminologyPropertiesList :: Lens.Lens' ListTerminologiesResponse (Core.Maybe [TerminologyProperties])
listTerminologiesResponse_terminologyPropertiesList = Lens.lens (\ListTerminologiesResponse' {terminologyPropertiesList} -> terminologyPropertiesList) (\s@ListTerminologiesResponse' {} a -> s {terminologyPropertiesList = a} :: ListTerminologiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTerminologiesResponse_httpStatus :: Lens.Lens' ListTerminologiesResponse Core.Int
listTerminologiesResponse_httpStatus = Lens.lens (\ListTerminologiesResponse' {httpStatus} -> httpStatus) (\s@ListTerminologiesResponse' {} a -> s {httpStatus = a} :: ListTerminologiesResponse)

instance Core.NFData ListTerminologiesResponse
