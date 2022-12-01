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
-- Module      : Amazonka.Rekognition.ListDatasetLabels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the labels in a dataset. Amazon Rekognition Custom Labels uses
-- labels to describe images. For more information, see
-- <https://docs.aws.amazon.com/rekognition/latest/customlabels-dg/md-labeling-images.html Labeling images>.
--
-- Lists the labels in a dataset. Amazon Rekognition Custom Labels uses
-- labels to describe images. For more information, see Labeling images in
-- the /Amazon Rekognition Custom Labels Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.Rekognition.ListDatasetLabels
  ( -- * Creating a Request
    ListDatasetLabels (..),
    newListDatasetLabels,

    -- * Request Lenses
    listDatasetLabels_nextToken,
    listDatasetLabels_maxResults,
    listDatasetLabels_datasetArn,

    -- * Destructuring the Response
    ListDatasetLabelsResponse (..),
    newListDatasetLabelsResponse,

    -- * Response Lenses
    listDatasetLabelsResponse_nextToken,
    listDatasetLabelsResponse_datasetLabelDescriptions,
    listDatasetLabelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasetLabels' smart constructor.
data ListDatasetLabels = ListDatasetLabels'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the dataset that you want to use.
    datasetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetLabels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetLabels_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'maxResults', 'listDatasetLabels_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
--
-- 'datasetArn', 'listDatasetLabels_datasetArn' - The Amazon Resource Name (ARN) of the dataset that you want to use.
newListDatasetLabels ::
  -- | 'datasetArn'
  Prelude.Text ->
  ListDatasetLabels
newListDatasetLabels pDatasetArn_ =
  ListDatasetLabels'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      datasetArn = pDatasetArn_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
listDatasetLabels_nextToken :: Lens.Lens' ListDatasetLabels (Prelude.Maybe Prelude.Text)
listDatasetLabels_nextToken = Lens.lens (\ListDatasetLabels' {nextToken} -> nextToken) (\s@ListDatasetLabels' {} a -> s {nextToken = a} :: ListDatasetLabels)

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
listDatasetLabels_maxResults :: Lens.Lens' ListDatasetLabels (Prelude.Maybe Prelude.Natural)
listDatasetLabels_maxResults = Lens.lens (\ListDatasetLabels' {maxResults} -> maxResults) (\s@ListDatasetLabels' {} a -> s {maxResults = a} :: ListDatasetLabels)

-- | The Amazon Resource Name (ARN) of the dataset that you want to use.
listDatasetLabels_datasetArn :: Lens.Lens' ListDatasetLabels Prelude.Text
listDatasetLabels_datasetArn = Lens.lens (\ListDatasetLabels' {datasetArn} -> datasetArn) (\s@ListDatasetLabels' {} a -> s {datasetArn = a} :: ListDatasetLabels)

instance Core.AWSPager ListDatasetLabels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetLabelsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetLabelsResponse_datasetLabelDescriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatasetLabels_nextToken
          Lens..~ rs
          Lens.^? listDatasetLabelsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDatasetLabels where
  type
    AWSResponse ListDatasetLabels =
      ListDatasetLabelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetLabelsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DatasetLabelDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasetLabels where
  hashWithSalt _salt ListDatasetLabels' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` datasetArn

instance Prelude.NFData ListDatasetLabels where
  rnf ListDatasetLabels' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf datasetArn

instance Core.ToHeaders ListDatasetLabels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.ListDatasetLabels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDatasetLabels where
  toJSON ListDatasetLabels' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("DatasetArn" Core..= datasetArn)
          ]
      )

instance Core.ToPath ListDatasetLabels where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDatasetLabels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetLabelsResponse' smart constructor.
data ListDatasetLabelsResponse = ListDatasetLabelsResponse'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the labels in the dataset.
    datasetLabelDescriptions :: Prelude.Maybe [DatasetLabelDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetLabelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetLabelsResponse_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'datasetLabelDescriptions', 'listDatasetLabelsResponse_datasetLabelDescriptions' - A list of the labels in the dataset.
--
-- 'httpStatus', 'listDatasetLabelsResponse_httpStatus' - The response's http status code.
newListDatasetLabelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetLabelsResponse
newListDatasetLabelsResponse pHttpStatus_ =
  ListDatasetLabelsResponse'
    { nextToken =
        Prelude.Nothing,
      datasetLabelDescriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
listDatasetLabelsResponse_nextToken :: Lens.Lens' ListDatasetLabelsResponse (Prelude.Maybe Prelude.Text)
listDatasetLabelsResponse_nextToken = Lens.lens (\ListDatasetLabelsResponse' {nextToken} -> nextToken) (\s@ListDatasetLabelsResponse' {} a -> s {nextToken = a} :: ListDatasetLabelsResponse)

-- | A list of the labels in the dataset.
listDatasetLabelsResponse_datasetLabelDescriptions :: Lens.Lens' ListDatasetLabelsResponse (Prelude.Maybe [DatasetLabelDescription])
listDatasetLabelsResponse_datasetLabelDescriptions = Lens.lens (\ListDatasetLabelsResponse' {datasetLabelDescriptions} -> datasetLabelDescriptions) (\s@ListDatasetLabelsResponse' {} a -> s {datasetLabelDescriptions = a} :: ListDatasetLabelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatasetLabelsResponse_httpStatus :: Lens.Lens' ListDatasetLabelsResponse Prelude.Int
listDatasetLabelsResponse_httpStatus = Lens.lens (\ListDatasetLabelsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetLabelsResponse' {} a -> s {httpStatus = a} :: ListDatasetLabelsResponse)

instance Prelude.NFData ListDatasetLabelsResponse where
  rnf ListDatasetLabelsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasetLabelDescriptions
      `Prelude.seq` Prelude.rnf httpStatus
