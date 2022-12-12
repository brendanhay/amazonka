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
-- Module      : Amazonka.Comprehend.ListEntityRecognizers
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Comprehend.ListEntityRecognizers
  ( -- * Creating a Request
    ListEntityRecognizers (..),
    newListEntityRecognizers,

    -- * Request Lenses
    listEntityRecognizers_filter,
    listEntityRecognizers_maxResults,
    listEntityRecognizers_nextToken,

    -- * Destructuring the Response
    ListEntityRecognizersResponse (..),
    newListEntityRecognizersResponse,

    -- * Response Lenses
    listEntityRecognizersResponse_entityRecognizerPropertiesList,
    listEntityRecognizersResponse_nextToken,
    listEntityRecognizersResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntityRecognizers' smart constructor.
data ListEntityRecognizers = ListEntityRecognizers'
  { -- | Filters the list of entities returned. You can filter on @Status@,
    -- @SubmitTimeBefore@, or @SubmitTimeAfter@. You can only set one filter at
    -- a time.
    filter' :: Prelude.Maybe EntityRecognizerFilter,
    -- | The maximum number of results to return on each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filter'', 'listEntityRecognizers_filter' - Filters the list of entities returned. You can filter on @Status@,
-- @SubmitTimeBefore@, or @SubmitTimeAfter@. You can only set one filter at
-- a time.
--
-- 'maxResults', 'listEntityRecognizers_maxResults' - The maximum number of results to return on each page. The default is
-- 100.
--
-- 'nextToken', 'listEntityRecognizers_nextToken' - Identifies the next page of results to return.
newListEntityRecognizers ::
  ListEntityRecognizers
newListEntityRecognizers =
  ListEntityRecognizers'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the list of entities returned. You can filter on @Status@,
-- @SubmitTimeBefore@, or @SubmitTimeAfter@. You can only set one filter at
-- a time.
listEntityRecognizers_filter :: Lens.Lens' ListEntityRecognizers (Prelude.Maybe EntityRecognizerFilter)
listEntityRecognizers_filter = Lens.lens (\ListEntityRecognizers' {filter'} -> filter') (\s@ListEntityRecognizers' {} a -> s {filter' = a} :: ListEntityRecognizers)

-- | The maximum number of results to return on each page. The default is
-- 100.
listEntityRecognizers_maxResults :: Lens.Lens' ListEntityRecognizers (Prelude.Maybe Prelude.Natural)
listEntityRecognizers_maxResults = Lens.lens (\ListEntityRecognizers' {maxResults} -> maxResults) (\s@ListEntityRecognizers' {} a -> s {maxResults = a} :: ListEntityRecognizers)

-- | Identifies the next page of results to return.
listEntityRecognizers_nextToken :: Lens.Lens' ListEntityRecognizers (Prelude.Maybe Prelude.Text)
listEntityRecognizers_nextToken = Lens.lens (\ListEntityRecognizers' {nextToken} -> nextToken) (\s@ListEntityRecognizers' {} a -> s {nextToken = a} :: ListEntityRecognizers)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntityRecognizersResponse'
            Prelude.<$> ( x Data..?> "EntityRecognizerPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntityRecognizers where
  hashWithSalt _salt ListEntityRecognizers' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEntityRecognizers where
  rnf ListEntityRecognizers' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEntityRecognizers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListEntityRecognizers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEntityRecognizers where
  toJSON ListEntityRecognizers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEntityRecognizers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEntityRecognizers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntityRecognizersResponse' smart constructor.
data ListEntityRecognizersResponse = ListEntityRecognizersResponse'
  { -- | The list of properties of an entity recognizer.
    entityRecognizerPropertiesList :: Prelude.Maybe [EntityRecognizerProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'entityRecognizerPropertiesList', 'listEntityRecognizersResponse_entityRecognizerPropertiesList' - The list of properties of an entity recognizer.
--
-- 'nextToken', 'listEntityRecognizersResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listEntityRecognizersResponse_httpStatus' - The response's http status code.
newListEntityRecognizersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntityRecognizersResponse
newListEntityRecognizersResponse pHttpStatus_ =
  ListEntityRecognizersResponse'
    { entityRecognizerPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of properties of an entity recognizer.
listEntityRecognizersResponse_entityRecognizerPropertiesList :: Lens.Lens' ListEntityRecognizersResponse (Prelude.Maybe [EntityRecognizerProperties])
listEntityRecognizersResponse_entityRecognizerPropertiesList = Lens.lens (\ListEntityRecognizersResponse' {entityRecognizerPropertiesList} -> entityRecognizerPropertiesList) (\s@ListEntityRecognizersResponse' {} a -> s {entityRecognizerPropertiesList = a} :: ListEntityRecognizersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listEntityRecognizersResponse_nextToken :: Lens.Lens' ListEntityRecognizersResponse (Prelude.Maybe Prelude.Text)
listEntityRecognizersResponse_nextToken = Lens.lens (\ListEntityRecognizersResponse' {nextToken} -> nextToken) (\s@ListEntityRecognizersResponse' {} a -> s {nextToken = a} :: ListEntityRecognizersResponse)

-- | The response's http status code.
listEntityRecognizersResponse_httpStatus :: Lens.Lens' ListEntityRecognizersResponse Prelude.Int
listEntityRecognizersResponse_httpStatus = Lens.lens (\ListEntityRecognizersResponse' {httpStatus} -> httpStatus) (\s@ListEntityRecognizersResponse' {} a -> s {httpStatus = a} :: ListEntityRecognizersResponse)

instance Prelude.NFData ListEntityRecognizersResponse where
  rnf ListEntityRecognizersResponse' {..} =
    Prelude.rnf entityRecognizerPropertiesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
