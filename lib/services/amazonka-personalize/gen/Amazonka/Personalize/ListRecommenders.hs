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
-- Module      : Amazonka.Personalize.ListRecommenders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of recommenders in a given Domain dataset group. When a
-- Domain dataset group is not specified, all the recommenders associated
-- with the account are listed. The response provides the properties for
-- each recommender, including the Amazon Resource Name (ARN). For more
-- information on recommenders, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateRecommender.html CreateRecommender>.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListRecommenders
  ( -- * Creating a Request
    ListRecommenders (..),
    newListRecommenders,

    -- * Request Lenses
    listRecommenders_datasetGroupArn,
    listRecommenders_maxResults,
    listRecommenders_nextToken,

    -- * Destructuring the Response
    ListRecommendersResponse (..),
    newListRecommendersResponse,

    -- * Response Lenses
    listRecommendersResponse_nextToken,
    listRecommendersResponse_recommenders,
    listRecommendersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecommenders' smart constructor.
data ListRecommenders = ListRecommenders'
  { -- | The Amazon Resource Name (ARN) of the Domain dataset group to list the
    -- recommenders for. When a Domain dataset group is not specified, all the
    -- recommenders associated with the account are listed.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of recommenders to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token returned from the previous call to @ListRecommenders@ for
    -- getting the next set of recommenders (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommenders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetGroupArn', 'listRecommenders_datasetGroupArn' - The Amazon Resource Name (ARN) of the Domain dataset group to list the
-- recommenders for. When a Domain dataset group is not specified, all the
-- recommenders associated with the account are listed.
--
-- 'maxResults', 'listRecommenders_maxResults' - The maximum number of recommenders to return.
--
-- 'nextToken', 'listRecommenders_nextToken' - A token returned from the previous call to @ListRecommenders@ for
-- getting the next set of recommenders (if they exist).
newListRecommenders ::
  ListRecommenders
newListRecommenders =
  ListRecommenders'
    { datasetGroupArn =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Domain dataset group to list the
-- recommenders for. When a Domain dataset group is not specified, all the
-- recommenders associated with the account are listed.
listRecommenders_datasetGroupArn :: Lens.Lens' ListRecommenders (Prelude.Maybe Prelude.Text)
listRecommenders_datasetGroupArn = Lens.lens (\ListRecommenders' {datasetGroupArn} -> datasetGroupArn) (\s@ListRecommenders' {} a -> s {datasetGroupArn = a} :: ListRecommenders)

-- | The maximum number of recommenders to return.
listRecommenders_maxResults :: Lens.Lens' ListRecommenders (Prelude.Maybe Prelude.Natural)
listRecommenders_maxResults = Lens.lens (\ListRecommenders' {maxResults} -> maxResults) (\s@ListRecommenders' {} a -> s {maxResults = a} :: ListRecommenders)

-- | A token returned from the previous call to @ListRecommenders@ for
-- getting the next set of recommenders (if they exist).
listRecommenders_nextToken :: Lens.Lens' ListRecommenders (Prelude.Maybe Prelude.Text)
listRecommenders_nextToken = Lens.lens (\ListRecommenders' {nextToken} -> nextToken) (\s@ListRecommenders' {} a -> s {nextToken = a} :: ListRecommenders)

instance Core.AWSPager ListRecommenders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecommendersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecommendersResponse_recommenders
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRecommenders_nextToken
          Lens..~ rs
          Lens.^? listRecommendersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRecommenders where
  type
    AWSResponse ListRecommenders =
      ListRecommendersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecommendersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "recommenders" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecommenders where
  hashWithSalt _salt ListRecommenders' {..} =
    _salt
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRecommenders where
  rnf ListRecommenders' {..} =
    Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListRecommenders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListRecommenders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRecommenders where
  toJSON ListRecommenders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("datasetGroupArn" Data..=)
              Prelude.<$> datasetGroupArn,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListRecommenders where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRecommenders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecommendersResponse' smart constructor.
data ListRecommendersResponse = ListRecommendersResponse'
  { -- | A token for getting the next set of recommenders (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the recommenders.
    recommenders :: Prelude.Maybe [RecommenderSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecommendersResponse_nextToken' - A token for getting the next set of recommenders (if they exist).
--
-- 'recommenders', 'listRecommendersResponse_recommenders' - A list of the recommenders.
--
-- 'httpStatus', 'listRecommendersResponse_httpStatus' - The response's http status code.
newListRecommendersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecommendersResponse
newListRecommendersResponse pHttpStatus_ =
  ListRecommendersResponse'
    { nextToken =
        Prelude.Nothing,
      recommenders = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of recommenders (if they exist).
listRecommendersResponse_nextToken :: Lens.Lens' ListRecommendersResponse (Prelude.Maybe Prelude.Text)
listRecommendersResponse_nextToken = Lens.lens (\ListRecommendersResponse' {nextToken} -> nextToken) (\s@ListRecommendersResponse' {} a -> s {nextToken = a} :: ListRecommendersResponse)

-- | A list of the recommenders.
listRecommendersResponse_recommenders :: Lens.Lens' ListRecommendersResponse (Prelude.Maybe [RecommenderSummary])
listRecommendersResponse_recommenders = Lens.lens (\ListRecommendersResponse' {recommenders} -> recommenders) (\s@ListRecommendersResponse' {} a -> s {recommenders = a} :: ListRecommendersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecommendersResponse_httpStatus :: Lens.Lens' ListRecommendersResponse Prelude.Int
listRecommendersResponse_httpStatus = Lens.lens (\ListRecommendersResponse' {httpStatus} -> httpStatus) (\s@ListRecommendersResponse' {} a -> s {httpStatus = a} :: ListRecommendersResponse)

instance Prelude.NFData ListRecommendersResponse where
  rnf ListRecommendersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommenders
      `Prelude.seq` Prelude.rnf httpStatus
