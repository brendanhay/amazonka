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
-- Module      : Amazonka.LookoutEquipment.ListDatasets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all datasets currently available in your account, filtering on the
-- dataset name.
module Amazonka.LookoutEquipment.ListDatasets
  ( -- * Creating a Request
    ListDatasets (..),
    newListDatasets,

    -- * Request Lenses
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasets_datasetNameBeginsWith,

    -- * Destructuring the Response
    ListDatasetsResponse (..),
    newListDatasetsResponse,

    -- * Response Lenses
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasetSummaries,
    listDatasetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | An opaque pagination token indicating where to continue the listing of
    -- datasets.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of datasets to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The beginning of the name of the datasets to be listed.
    datasetNameBeginsWith :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasets_nextToken' - An opaque pagination token indicating where to continue the listing of
-- datasets.
--
-- 'maxResults', 'listDatasets_maxResults' - Specifies the maximum number of datasets to list.
--
-- 'datasetNameBeginsWith', 'listDatasets_datasetNameBeginsWith' - The beginning of the name of the datasets to be listed.
newListDatasets ::
  ListDatasets
newListDatasets =
  ListDatasets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      datasetNameBeginsWith = Prelude.Nothing
    }

-- | An opaque pagination token indicating where to continue the listing of
-- datasets.
listDatasets_nextToken :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Text)
listDatasets_nextToken = Lens.lens (\ListDatasets' {nextToken} -> nextToken) (\s@ListDatasets' {} a -> s {nextToken = a} :: ListDatasets)

-- | Specifies the maximum number of datasets to list.
listDatasets_maxResults :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Natural)
listDatasets_maxResults = Lens.lens (\ListDatasets' {maxResults} -> maxResults) (\s@ListDatasets' {} a -> s {maxResults = a} :: ListDatasets)

-- | The beginning of the name of the datasets to be listed.
listDatasets_datasetNameBeginsWith :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Text)
listDatasets_datasetNameBeginsWith = Lens.lens (\ListDatasets' {datasetNameBeginsWith} -> datasetNameBeginsWith) (\s@ListDatasets' {} a -> s {datasetNameBeginsWith = a} :: ListDatasets)

instance Core.AWSRequest ListDatasets where
  type AWSResponse ListDatasets = ListDatasetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "DatasetSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasets where
  hashWithSalt _salt ListDatasets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` datasetNameBeginsWith

instance Prelude.NFData ListDatasets where
  rnf ListDatasets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf datasetNameBeginsWith

instance Data.ToHeaders ListDatasets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.ListDatasets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatasets where
  toJSON ListDatasets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("DatasetNameBeginsWith" Data..=)
              Prelude.<$> datasetNameBeginsWith
          ]
      )

instance Data.ToPath ListDatasets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDatasets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | An opaque pagination token indicating where to continue the listing of
    -- datasets.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the specified dataset, including creation
    -- time, dataset ARN, and status.
    datasetSummaries :: Prelude.Maybe [DatasetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetsResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- datasets.
--
-- 'datasetSummaries', 'listDatasetsResponse_datasetSummaries' - Provides information about the specified dataset, including creation
-- time, dataset ARN, and status.
--
-- 'httpStatus', 'listDatasetsResponse_httpStatus' - The response's http status code.
newListDatasetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetsResponse
newListDatasetsResponse pHttpStatus_ =
  ListDatasetsResponse'
    { nextToken = Prelude.Nothing,
      datasetSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token indicating where to continue the listing of
-- datasets.
listDatasetsResponse_nextToken :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe Prelude.Text)
listDatasetsResponse_nextToken = Lens.lens (\ListDatasetsResponse' {nextToken} -> nextToken) (\s@ListDatasetsResponse' {} a -> s {nextToken = a} :: ListDatasetsResponse)

-- | Provides information about the specified dataset, including creation
-- time, dataset ARN, and status.
listDatasetsResponse_datasetSummaries :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe [DatasetSummary])
listDatasetsResponse_datasetSummaries = Lens.lens (\ListDatasetsResponse' {datasetSummaries} -> datasetSummaries) (\s@ListDatasetsResponse' {} a -> s {datasetSummaries = a} :: ListDatasetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatasetsResponse_httpStatus :: Lens.Lens' ListDatasetsResponse Prelude.Int
listDatasetsResponse_httpStatus = Lens.lens (\ListDatasetsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetsResponse' {} a -> s {httpStatus = a} :: ListDatasetsResponse)

instance Prelude.NFData ListDatasetsResponse where
  rnf ListDatasetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasetSummaries
      `Prelude.seq` Prelude.rnf httpStatus
