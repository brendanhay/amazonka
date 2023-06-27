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
-- Module      : Amazonka.Comprehend.ListDatasets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the datasets that you have configured in this Region. For more
-- information about datasets, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.ListDatasets
  ( -- * Creating a Request
    ListDatasets (..),
    newListDatasets,

    -- * Request Lenses
    listDatasets_filter,
    listDatasets_flywheelArn,
    listDatasets_maxResults,
    listDatasets_nextToken,

    -- * Destructuring the Response
    ListDatasetsResponse (..),
    newListDatasetsResponse,

    -- * Response Lenses
    listDatasetsResponse_datasetPropertiesList,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | Filters the datasets to be returned in the response.
    filter' :: Prelude.Maybe DatasetFilter,
    -- | The Amazon Resource Number (ARN) of the flywheel.
    flywheelArn :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return in a response. The default is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filter'', 'listDatasets_filter' - Filters the datasets to be returned in the response.
--
-- 'flywheelArn', 'listDatasets_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel.
--
-- 'maxResults', 'listDatasets_maxResults' - Maximum number of results to return in a response. The default is 100.
--
-- 'nextToken', 'listDatasets_nextToken' - Identifies the next page of results to return.
newListDatasets ::
  ListDatasets
newListDatasets =
  ListDatasets'
    { filter' = Prelude.Nothing,
      flywheelArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the datasets to be returned in the response.
listDatasets_filter :: Lens.Lens' ListDatasets (Prelude.Maybe DatasetFilter)
listDatasets_filter = Lens.lens (\ListDatasets' {filter'} -> filter') (\s@ListDatasets' {} a -> s {filter' = a} :: ListDatasets)

-- | The Amazon Resource Number (ARN) of the flywheel.
listDatasets_flywheelArn :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Text)
listDatasets_flywheelArn = Lens.lens (\ListDatasets' {flywheelArn} -> flywheelArn) (\s@ListDatasets' {} a -> s {flywheelArn = a} :: ListDatasets)

-- | Maximum number of results to return in a response. The default is 100.
listDatasets_maxResults :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Natural)
listDatasets_maxResults = Lens.lens (\ListDatasets' {maxResults} -> maxResults) (\s@ListDatasets' {} a -> s {maxResults = a} :: ListDatasets)

-- | Identifies the next page of results to return.
listDatasets_nextToken :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Text)
listDatasets_nextToken = Lens.lens (\ListDatasets' {nextToken} -> nextToken) (\s@ListDatasets' {} a -> s {nextToken = a} :: ListDatasets)

instance Core.AWSRequest ListDatasets where
  type AWSResponse ListDatasets = ListDatasetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Prelude.<$> ( x
                            Data..?> "DatasetPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasets where
  hashWithSalt _salt ListDatasets' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` flywheelArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDatasets where
  rnf ListDatasets' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDatasets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListDatasets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatasets where
  toJSON ListDatasets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("FlywheelArn" Data..=) Prelude.<$> flywheelArn,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDatasets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDatasets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | The dataset properties list.
    datasetPropertiesList :: Prelude.Maybe [DatasetProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'datasetPropertiesList', 'listDatasetsResponse_datasetPropertiesList' - The dataset properties list.
--
-- 'nextToken', 'listDatasetsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listDatasetsResponse_httpStatus' - The response's http status code.
newListDatasetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetsResponse
newListDatasetsResponse pHttpStatus_ =
  ListDatasetsResponse'
    { datasetPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The dataset properties list.
listDatasetsResponse_datasetPropertiesList :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe [DatasetProperties])
listDatasetsResponse_datasetPropertiesList = Lens.lens (\ListDatasetsResponse' {datasetPropertiesList} -> datasetPropertiesList) (\s@ListDatasetsResponse' {} a -> s {datasetPropertiesList = a} :: ListDatasetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listDatasetsResponse_nextToken :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe Prelude.Text)
listDatasetsResponse_nextToken = Lens.lens (\ListDatasetsResponse' {nextToken} -> nextToken) (\s@ListDatasetsResponse' {} a -> s {nextToken = a} :: ListDatasetsResponse)

-- | The response's http status code.
listDatasetsResponse_httpStatus :: Lens.Lens' ListDatasetsResponse Prelude.Int
listDatasetsResponse_httpStatus = Lens.lens (\ListDatasetsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetsResponse' {} a -> s {httpStatus = a} :: ListDatasetsResponse)

instance Prelude.NFData ListDatasetsResponse where
  rnf ListDatasetsResponse' {..} =
    Prelude.rnf datasetPropertiesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
