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
-- Module      : Network.AWS.Comprehend.ListEntityRecognizers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the properties of all entity recognizers that you
-- created, including recognizers currently in training. Allows you to
-- filter the list of recognizers based on criteria such as status and
-- submission time. This call returns up to 500 entity recognizers in the
-- list, with a default number of 100 recognizers in the list.
--
-- The results of this list are not in any particular order. Please get the
-- list and sort locally if needed.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntityRecognizers
  ( -- * Creating a Request
    ListEntityRecognizers (..),
    newListEntityRecognizers,

    -- * Request Lenses
    listEntityRecognizers_nextToken,
    listEntityRecognizers_maxResults,
    listEntityRecognizers_filter,

    -- * Destructuring the Response
    ListEntityRecognizersResponse (..),
    newListEntityRecognizersResponse,

    -- * Response Lenses
    listEntityRecognizersResponse_nextToken,
    listEntityRecognizersResponse_entityRecognizerPropertiesList,
    listEntityRecognizersResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEntityRecognizers' smart constructor.
data ListEntityRecognizers = ListEntityRecognizers'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return on each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the list of entities returned. You can filter on @Status@,
    -- @SubmitTimeBefore@, or @SubmitTimeAfter@. You can only set one filter at
    -- a time.
    filter' :: Prelude.Maybe EntityRecognizerFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntityRecognizers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntityRecognizers_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listEntityRecognizers_maxResults' - The maximum number of results to return on each page. The default is
-- 100.
--
-- 'filter'', 'listEntityRecognizers_filter' - Filters the list of entities returned. You can filter on @Status@,
-- @SubmitTimeBefore@, or @SubmitTimeAfter@. You can only set one filter at
-- a time.
newListEntityRecognizers ::
  ListEntityRecognizers
newListEntityRecognizers =
  ListEntityRecognizers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listEntityRecognizers_nextToken :: Lens.Lens' ListEntityRecognizers (Prelude.Maybe Prelude.Text)
listEntityRecognizers_nextToken = Lens.lens (\ListEntityRecognizers' {nextToken} -> nextToken) (\s@ListEntityRecognizers' {} a -> s {nextToken = a} :: ListEntityRecognizers)

-- | The maximum number of results to return on each page. The default is
-- 100.
listEntityRecognizers_maxResults :: Lens.Lens' ListEntityRecognizers (Prelude.Maybe Prelude.Natural)
listEntityRecognizers_maxResults = Lens.lens (\ListEntityRecognizers' {maxResults} -> maxResults) (\s@ListEntityRecognizers' {} a -> s {maxResults = a} :: ListEntityRecognizers)

-- | Filters the list of entities returned. You can filter on @Status@,
-- @SubmitTimeBefore@, or @SubmitTimeAfter@. You can only set one filter at
-- a time.
listEntityRecognizers_filter :: Lens.Lens' ListEntityRecognizers (Prelude.Maybe EntityRecognizerFilter)
listEntityRecognizers_filter = Lens.lens (\ListEntityRecognizers' {filter'} -> filter') (\s@ListEntityRecognizers' {} a -> s {filter' = a} :: ListEntityRecognizers)

instance Core.AWSPager ListEntityRecognizers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEntityRecognizersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEntityRecognizersResponse_entityRecognizerPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEntityRecognizers_nextToken
          Lens..~ rs
          Lens.^? listEntityRecognizersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEntityRecognizers where
  type
    AWSResponse ListEntityRecognizers =
      ListEntityRecognizersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntityRecognizersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "EntityRecognizerPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntityRecognizers

instance Prelude.NFData ListEntityRecognizers

instance Core.ToHeaders ListEntityRecognizers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListEntityRecognizers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEntityRecognizers where
  toJSON ListEntityRecognizers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filter" Core..=) Prelude.<$> filter'
          ]
      )

instance Core.ToPath ListEntityRecognizers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEntityRecognizers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntityRecognizersResponse' smart constructor.
data ListEntityRecognizersResponse = ListEntityRecognizersResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of properties of an entity recognizer.
    entityRecognizerPropertiesList :: Prelude.Maybe [EntityRecognizerProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntityRecognizersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntityRecognizersResponse_nextToken' - Identifies the next page of results to return.
--
-- 'entityRecognizerPropertiesList', 'listEntityRecognizersResponse_entityRecognizerPropertiesList' - The list of properties of an entity recognizer.
--
-- 'httpStatus', 'listEntityRecognizersResponse_httpStatus' - The response's http status code.
newListEntityRecognizersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntityRecognizersResponse
newListEntityRecognizersResponse pHttpStatus_ =
  ListEntityRecognizersResponse'
    { nextToken =
        Prelude.Nothing,
      entityRecognizerPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listEntityRecognizersResponse_nextToken :: Lens.Lens' ListEntityRecognizersResponse (Prelude.Maybe Prelude.Text)
listEntityRecognizersResponse_nextToken = Lens.lens (\ListEntityRecognizersResponse' {nextToken} -> nextToken) (\s@ListEntityRecognizersResponse' {} a -> s {nextToken = a} :: ListEntityRecognizersResponse)

-- | The list of properties of an entity recognizer.
listEntityRecognizersResponse_entityRecognizerPropertiesList :: Lens.Lens' ListEntityRecognizersResponse (Prelude.Maybe [EntityRecognizerProperties])
listEntityRecognizersResponse_entityRecognizerPropertiesList = Lens.lens (\ListEntityRecognizersResponse' {entityRecognizerPropertiesList} -> entityRecognizerPropertiesList) (\s@ListEntityRecognizersResponse' {} a -> s {entityRecognizerPropertiesList = a} :: ListEntityRecognizersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listEntityRecognizersResponse_httpStatus :: Lens.Lens' ListEntityRecognizersResponse Prelude.Int
listEntityRecognizersResponse_httpStatus = Lens.lens (\ListEntityRecognizersResponse' {httpStatus} -> httpStatus) (\s@ListEntityRecognizersResponse' {} a -> s {httpStatus = a} :: ListEntityRecognizersResponse)

instance Prelude.NFData ListEntityRecognizersResponse
