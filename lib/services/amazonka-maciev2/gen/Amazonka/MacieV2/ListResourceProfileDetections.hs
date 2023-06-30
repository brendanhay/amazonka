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
-- Module      : Amazonka.MacieV2.ListResourceProfileDetections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the types and amount of sensitive data that
-- Amazon Macie found in an S3 bucket.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListResourceProfileDetections
  ( -- * Creating a Request
    ListResourceProfileDetections (..),
    newListResourceProfileDetections,

    -- * Request Lenses
    listResourceProfileDetections_maxResults,
    listResourceProfileDetections_nextToken,
    listResourceProfileDetections_resourceArn,

    -- * Destructuring the Response
    ListResourceProfileDetectionsResponse (..),
    newListResourceProfileDetectionsResponse,

    -- * Response Lenses
    listResourceProfileDetectionsResponse_detections,
    listResourceProfileDetectionsResponse_nextToken,
    listResourceProfileDetectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceProfileDetections' smart constructor.
data ListResourceProfileDetections = ListResourceProfileDetections'
  { -- | The maximum number of items to include in each page of a paginated
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
    -- to.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceProfileDetections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResourceProfileDetections_maxResults' - The maximum number of items to include in each page of a paginated
-- response.
--
-- 'nextToken', 'listResourceProfileDetections_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'resourceArn', 'listResourceProfileDetections_resourceArn' - The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
newListResourceProfileDetections ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListResourceProfileDetections
newListResourceProfileDetections pResourceArn_ =
  ListResourceProfileDetections'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The maximum number of items to include in each page of a paginated
-- response.
listResourceProfileDetections_maxResults :: Lens.Lens' ListResourceProfileDetections (Prelude.Maybe Prelude.Natural)
listResourceProfileDetections_maxResults = Lens.lens (\ListResourceProfileDetections' {maxResults} -> maxResults) (\s@ListResourceProfileDetections' {} a -> s {maxResults = a} :: ListResourceProfileDetections)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listResourceProfileDetections_nextToken :: Lens.Lens' ListResourceProfileDetections (Prelude.Maybe Prelude.Text)
listResourceProfileDetections_nextToken = Lens.lens (\ListResourceProfileDetections' {nextToken} -> nextToken) (\s@ListResourceProfileDetections' {} a -> s {nextToken = a} :: ListResourceProfileDetections)

-- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
listResourceProfileDetections_resourceArn :: Lens.Lens' ListResourceProfileDetections Prelude.Text
listResourceProfileDetections_resourceArn = Lens.lens (\ListResourceProfileDetections' {resourceArn} -> resourceArn) (\s@ListResourceProfileDetections' {} a -> s {resourceArn = a} :: ListResourceProfileDetections)

instance Core.AWSPager ListResourceProfileDetections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceProfileDetectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceProfileDetectionsResponse_detections
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listResourceProfileDetections_nextToken
          Lens..~ rs
          Lens.^? listResourceProfileDetectionsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListResourceProfileDetections
  where
  type
    AWSResponse ListResourceProfileDetections =
      ListResourceProfileDetectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceProfileDetectionsResponse'
            Prelude.<$> (x Data..?> "detections" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResourceProfileDetections
  where
  hashWithSalt _salt ListResourceProfileDetections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ListResourceProfileDetections where
  rnf ListResourceProfileDetections' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders ListResourceProfileDetections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListResourceProfileDetections where
  toPath =
    Prelude.const "/resource-profiles/detections"

instance Data.ToQuery ListResourceProfileDetections where
  toQuery ListResourceProfileDetections' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "resourceArn" Data.=: resourceArn
      ]

-- | /See:/ 'newListResourceProfileDetectionsResponse' smart constructor.
data ListResourceProfileDetectionsResponse = ListResourceProfileDetectionsResponse'
  { -- | An array of objects, one for each type of sensitive data that Amazon
    -- Macie found in the bucket. Each object reports the number of occurrences
    -- of the specified type and provides information about the custom data
    -- identifier or managed data identifier that detected the data.
    detections :: Prelude.Maybe [Detection],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceProfileDetectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detections', 'listResourceProfileDetectionsResponse_detections' - An array of objects, one for each type of sensitive data that Amazon
-- Macie found in the bucket. Each object reports the number of occurrences
-- of the specified type and provides information about the custom data
-- identifier or managed data identifier that detected the data.
--
-- 'nextToken', 'listResourceProfileDetectionsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listResourceProfileDetectionsResponse_httpStatus' - The response's http status code.
newListResourceProfileDetectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceProfileDetectionsResponse
newListResourceProfileDetectionsResponse pHttpStatus_ =
  ListResourceProfileDetectionsResponse'
    { detections =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each type of sensitive data that Amazon
-- Macie found in the bucket. Each object reports the number of occurrences
-- of the specified type and provides information about the custom data
-- identifier or managed data identifier that detected the data.
listResourceProfileDetectionsResponse_detections :: Lens.Lens' ListResourceProfileDetectionsResponse (Prelude.Maybe [Detection])
listResourceProfileDetectionsResponse_detections = Lens.lens (\ListResourceProfileDetectionsResponse' {detections} -> detections) (\s@ListResourceProfileDetectionsResponse' {} a -> s {detections = a} :: ListResourceProfileDetectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listResourceProfileDetectionsResponse_nextToken :: Lens.Lens' ListResourceProfileDetectionsResponse (Prelude.Maybe Prelude.Text)
listResourceProfileDetectionsResponse_nextToken = Lens.lens (\ListResourceProfileDetectionsResponse' {nextToken} -> nextToken) (\s@ListResourceProfileDetectionsResponse' {} a -> s {nextToken = a} :: ListResourceProfileDetectionsResponse)

-- | The response's http status code.
listResourceProfileDetectionsResponse_httpStatus :: Lens.Lens' ListResourceProfileDetectionsResponse Prelude.Int
listResourceProfileDetectionsResponse_httpStatus = Lens.lens (\ListResourceProfileDetectionsResponse' {httpStatus} -> httpStatus) (\s@ListResourceProfileDetectionsResponse' {} a -> s {httpStatus = a} :: ListResourceProfileDetectionsResponse)

instance
  Prelude.NFData
    ListResourceProfileDetectionsResponse
  where
  rnf ListResourceProfileDetectionsResponse' {..} =
    Prelude.rnf detections
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
