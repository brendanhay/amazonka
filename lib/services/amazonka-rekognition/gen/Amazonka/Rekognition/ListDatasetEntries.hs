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
-- Module      : Amazonka.Rekognition.ListDatasetEntries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the entries (images) within a dataset. An entry is a JSON Line
-- that contains the information for a single image, including the image
-- location, assigned labels, and object location bounding boxes. For more
-- information, see
-- <https://docs.aws.amazon.com/rekognition/latest/customlabels-dg/md-manifest-files.html Creating a manifest file>.
--
-- JSON Lines in the response include information about non-terminal errors
-- found in the dataset. Non terminal errors are reported in @errors@ lists
-- within each JSON Line. The same information is reported in the training
-- and testing validation result manifests that Amazon Rekognition Custom
-- Labels creates during model training.
--
-- You can filter the response in variety of ways, such as choosing which
-- labels to return and returning JSON Lines created after a specific date.
--
-- This operation requires permissions to perform the
-- @rekognition:ListDatasetEntries@ action.
--
-- This operation returns paginated results.
module Amazonka.Rekognition.ListDatasetEntries
  ( -- * Creating a Request
    ListDatasetEntries (..),
    newListDatasetEntries,

    -- * Request Lenses
    listDatasetEntries_nextToken,
    listDatasetEntries_labeled,
    listDatasetEntries_containsLabels,
    listDatasetEntries_sourceRefContains,
    listDatasetEntries_hasErrors,
    listDatasetEntries_maxResults,
    listDatasetEntries_datasetArn,

    -- * Destructuring the Response
    ListDatasetEntriesResponse (..),
    newListDatasetEntriesResponse,

    -- * Response Lenses
    listDatasetEntriesResponse_nextToken,
    listDatasetEntriesResponse_datasetEntries,
    listDatasetEntriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasetEntries' smart constructor.
data ListDatasetEntries = ListDatasetEntries'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify @true@ to get only the JSON Lines where the image is labeled.
    -- Specify @false@ to get only the JSON Lines where the image isn\'t
    -- labeled. If you don\'t specify @Labeled@, @ListDatasetEntries@ returns
    -- JSON Lines for labeled and unlabeled images.
    labeled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a label filter for the response. The response includes an
    -- entry only if one or more of the labels in @ContainsLabels@ exist in the
    -- entry.
    containsLabels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | If specified, @ListDatasetEntries@ only returns JSON Lines where the
    -- value of @SourceRefContains@ is part of the @source-ref@ field. The
    -- @source-ref@ field contains the Amazon S3 location of the image. You can
    -- use @SouceRefContains@ for tasks such as getting the JSON Line for a
    -- single image, or gettting JSON Lines for all images within a specific
    -- folder.
    sourceRefContains :: Prelude.Maybe Prelude.Text,
    -- | Specifies an error filter for the response. Specify @True@ to only
    -- include entries that have errors.
    hasErrors :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) for the dataset that you want to use.
    datasetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetEntries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetEntries_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'labeled', 'listDatasetEntries_labeled' - Specify @true@ to get only the JSON Lines where the image is labeled.
-- Specify @false@ to get only the JSON Lines where the image isn\'t
-- labeled. If you don\'t specify @Labeled@, @ListDatasetEntries@ returns
-- JSON Lines for labeled and unlabeled images.
--
-- 'containsLabels', 'listDatasetEntries_containsLabels' - Specifies a label filter for the response. The response includes an
-- entry only if one or more of the labels in @ContainsLabels@ exist in the
-- entry.
--
-- 'sourceRefContains', 'listDatasetEntries_sourceRefContains' - If specified, @ListDatasetEntries@ only returns JSON Lines where the
-- value of @SourceRefContains@ is part of the @source-ref@ field. The
-- @source-ref@ field contains the Amazon S3 location of the image. You can
-- use @SouceRefContains@ for tasks such as getting the JSON Line for a
-- single image, or gettting JSON Lines for all images within a specific
-- folder.
--
-- 'hasErrors', 'listDatasetEntries_hasErrors' - Specifies an error filter for the response. Specify @True@ to only
-- include entries that have errors.
--
-- 'maxResults', 'listDatasetEntries_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
--
-- 'datasetArn', 'listDatasetEntries_datasetArn' - The Amazon Resource Name (ARN) for the dataset that you want to use.
newListDatasetEntries ::
  -- | 'datasetArn'
  Prelude.Text ->
  ListDatasetEntries
newListDatasetEntries pDatasetArn_ =
  ListDatasetEntries'
    { nextToken = Prelude.Nothing,
      labeled = Prelude.Nothing,
      containsLabels = Prelude.Nothing,
      sourceRefContains = Prelude.Nothing,
      hasErrors = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      datasetArn = pDatasetArn_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
listDatasetEntries_nextToken :: Lens.Lens' ListDatasetEntries (Prelude.Maybe Prelude.Text)
listDatasetEntries_nextToken = Lens.lens (\ListDatasetEntries' {nextToken} -> nextToken) (\s@ListDatasetEntries' {} a -> s {nextToken = a} :: ListDatasetEntries)

-- | Specify @true@ to get only the JSON Lines where the image is labeled.
-- Specify @false@ to get only the JSON Lines where the image isn\'t
-- labeled. If you don\'t specify @Labeled@, @ListDatasetEntries@ returns
-- JSON Lines for labeled and unlabeled images.
listDatasetEntries_labeled :: Lens.Lens' ListDatasetEntries (Prelude.Maybe Prelude.Bool)
listDatasetEntries_labeled = Lens.lens (\ListDatasetEntries' {labeled} -> labeled) (\s@ListDatasetEntries' {} a -> s {labeled = a} :: ListDatasetEntries)

-- | Specifies a label filter for the response. The response includes an
-- entry only if one or more of the labels in @ContainsLabels@ exist in the
-- entry.
listDatasetEntries_containsLabels :: Lens.Lens' ListDatasetEntries (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listDatasetEntries_containsLabels = Lens.lens (\ListDatasetEntries' {containsLabels} -> containsLabels) (\s@ListDatasetEntries' {} a -> s {containsLabels = a} :: ListDatasetEntries) Prelude.. Lens.mapping Lens.coerced

-- | If specified, @ListDatasetEntries@ only returns JSON Lines where the
-- value of @SourceRefContains@ is part of the @source-ref@ field. The
-- @source-ref@ field contains the Amazon S3 location of the image. You can
-- use @SouceRefContains@ for tasks such as getting the JSON Line for a
-- single image, or gettting JSON Lines for all images within a specific
-- folder.
listDatasetEntries_sourceRefContains :: Lens.Lens' ListDatasetEntries (Prelude.Maybe Prelude.Text)
listDatasetEntries_sourceRefContains = Lens.lens (\ListDatasetEntries' {sourceRefContains} -> sourceRefContains) (\s@ListDatasetEntries' {} a -> s {sourceRefContains = a} :: ListDatasetEntries)

-- | Specifies an error filter for the response. Specify @True@ to only
-- include entries that have errors.
listDatasetEntries_hasErrors :: Lens.Lens' ListDatasetEntries (Prelude.Maybe Prelude.Bool)
listDatasetEntries_hasErrors = Lens.lens (\ListDatasetEntries' {hasErrors} -> hasErrors) (\s@ListDatasetEntries' {} a -> s {hasErrors = a} :: ListDatasetEntries)

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
listDatasetEntries_maxResults :: Lens.Lens' ListDatasetEntries (Prelude.Maybe Prelude.Natural)
listDatasetEntries_maxResults = Lens.lens (\ListDatasetEntries' {maxResults} -> maxResults) (\s@ListDatasetEntries' {} a -> s {maxResults = a} :: ListDatasetEntries)

-- | The Amazon Resource Name (ARN) for the dataset that you want to use.
listDatasetEntries_datasetArn :: Lens.Lens' ListDatasetEntries Prelude.Text
listDatasetEntries_datasetArn = Lens.lens (\ListDatasetEntries' {datasetArn} -> datasetArn) (\s@ListDatasetEntries' {} a -> s {datasetArn = a} :: ListDatasetEntries)

instance Core.AWSPager ListDatasetEntries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetEntriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetEntriesResponse_datasetEntries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatasetEntries_nextToken
          Lens..~ rs
          Lens.^? listDatasetEntriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDatasetEntries where
  type
    AWSResponse ListDatasetEntries =
      ListDatasetEntriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetEntriesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "DatasetEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasetEntries where
  hashWithSalt _salt ListDatasetEntries' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` labeled
      `Prelude.hashWithSalt` containsLabels
      `Prelude.hashWithSalt` sourceRefContains
      `Prelude.hashWithSalt` hasErrors
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` datasetArn

instance Prelude.NFData ListDatasetEntries where
  rnf ListDatasetEntries' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf labeled
      `Prelude.seq` Prelude.rnf containsLabels
      `Prelude.seq` Prelude.rnf sourceRefContains
      `Prelude.seq` Prelude.rnf hasErrors
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf datasetArn

instance Core.ToHeaders ListDatasetEntries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.ListDatasetEntries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDatasetEntries where
  toJSON ListDatasetEntries' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Labeled" Core..=) Prelude.<$> labeled,
            ("ContainsLabels" Core..=)
              Prelude.<$> containsLabels,
            ("SourceRefContains" Core..=)
              Prelude.<$> sourceRefContains,
            ("HasErrors" Core..=) Prelude.<$> hasErrors,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("DatasetArn" Core..= datasetArn)
          ]
      )

instance Core.ToPath ListDatasetEntries where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDatasetEntries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetEntriesResponse' smart constructor.
data ListDatasetEntriesResponse = ListDatasetEntriesResponse'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of entries (images) in the dataset.
    datasetEntries :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetEntriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetEntriesResponse_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'datasetEntries', 'listDatasetEntriesResponse_datasetEntries' - A list of entries (images) in the dataset.
--
-- 'httpStatus', 'listDatasetEntriesResponse_httpStatus' - The response's http status code.
newListDatasetEntriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetEntriesResponse
newListDatasetEntriesResponse pHttpStatus_ =
  ListDatasetEntriesResponse'
    { nextToken =
        Prelude.Nothing,
      datasetEntries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
listDatasetEntriesResponse_nextToken :: Lens.Lens' ListDatasetEntriesResponse (Prelude.Maybe Prelude.Text)
listDatasetEntriesResponse_nextToken = Lens.lens (\ListDatasetEntriesResponse' {nextToken} -> nextToken) (\s@ListDatasetEntriesResponse' {} a -> s {nextToken = a} :: ListDatasetEntriesResponse)

-- | A list of entries (images) in the dataset.
listDatasetEntriesResponse_datasetEntries :: Lens.Lens' ListDatasetEntriesResponse (Prelude.Maybe [Prelude.Text])
listDatasetEntriesResponse_datasetEntries = Lens.lens (\ListDatasetEntriesResponse' {datasetEntries} -> datasetEntries) (\s@ListDatasetEntriesResponse' {} a -> s {datasetEntries = a} :: ListDatasetEntriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatasetEntriesResponse_httpStatus :: Lens.Lens' ListDatasetEntriesResponse Prelude.Int
listDatasetEntriesResponse_httpStatus = Lens.lens (\ListDatasetEntriesResponse' {httpStatus} -> httpStatus) (\s@ListDatasetEntriesResponse' {} a -> s {httpStatus = a} :: ListDatasetEntriesResponse)

instance Prelude.NFData ListDatasetEntriesResponse where
  rnf ListDatasetEntriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasetEntries
      `Prelude.seq` Prelude.rnf httpStatus
