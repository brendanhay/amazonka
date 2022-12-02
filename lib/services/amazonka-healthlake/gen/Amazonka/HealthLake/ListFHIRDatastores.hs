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
-- Module      : Amazonka.HealthLake.ListFHIRDatastores
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all FHIR Data Stores that are in the user’s account, regardless of
-- Data Store status.
module Amazonka.HealthLake.ListFHIRDatastores
  ( -- * Creating a Request
    ListFHIRDatastores (..),
    newListFHIRDatastores,

    -- * Request Lenses
    listFHIRDatastores_nextToken,
    listFHIRDatastores_filter,
    listFHIRDatastores_maxResults,

    -- * Destructuring the Response
    ListFHIRDatastoresResponse (..),
    newListFHIRDatastoresResponse,

    -- * Response Lenses
    listFHIRDatastoresResponse_nextToken,
    listFHIRDatastoresResponse_httpStatus,
    listFHIRDatastoresResponse_datastorePropertiesList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFHIRDatastores' smart constructor.
data ListFHIRDatastores = ListFHIRDatastores'
  { -- | Fetches the next page of Data Stores when results are paginated.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists all filters associated with a FHIR Data Store request.
    filter' :: Prelude.Maybe DatastoreFilter,
    -- | The maximum number of Data Stores returned in a single page of a
    -- ListFHIRDatastoresRequest call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFHIRDatastores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFHIRDatastores_nextToken' - Fetches the next page of Data Stores when results are paginated.
--
-- 'filter'', 'listFHIRDatastores_filter' - Lists all filters associated with a FHIR Data Store request.
--
-- 'maxResults', 'listFHIRDatastores_maxResults' - The maximum number of Data Stores returned in a single page of a
-- ListFHIRDatastoresRequest call.
newListFHIRDatastores ::
  ListFHIRDatastores
newListFHIRDatastores =
  ListFHIRDatastores'
    { nextToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Fetches the next page of Data Stores when results are paginated.
listFHIRDatastores_nextToken :: Lens.Lens' ListFHIRDatastores (Prelude.Maybe Prelude.Text)
listFHIRDatastores_nextToken = Lens.lens (\ListFHIRDatastores' {nextToken} -> nextToken) (\s@ListFHIRDatastores' {} a -> s {nextToken = a} :: ListFHIRDatastores)

-- | Lists all filters associated with a FHIR Data Store request.
listFHIRDatastores_filter :: Lens.Lens' ListFHIRDatastores (Prelude.Maybe DatastoreFilter)
listFHIRDatastores_filter = Lens.lens (\ListFHIRDatastores' {filter'} -> filter') (\s@ListFHIRDatastores' {} a -> s {filter' = a} :: ListFHIRDatastores)

-- | The maximum number of Data Stores returned in a single page of a
-- ListFHIRDatastoresRequest call.
listFHIRDatastores_maxResults :: Lens.Lens' ListFHIRDatastores (Prelude.Maybe Prelude.Natural)
listFHIRDatastores_maxResults = Lens.lens (\ListFHIRDatastores' {maxResults} -> maxResults) (\s@ListFHIRDatastores' {} a -> s {maxResults = a} :: ListFHIRDatastores)

instance Core.AWSRequest ListFHIRDatastores where
  type
    AWSResponse ListFHIRDatastores =
      ListFHIRDatastoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFHIRDatastoresResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "DatastorePropertiesList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFHIRDatastores where
  hashWithSalt _salt ListFHIRDatastores' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFHIRDatastores where
  rnf ListFHIRDatastores' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListFHIRDatastores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "HealthLake.ListFHIRDatastores" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFHIRDatastores where
  toJSON ListFHIRDatastores' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListFHIRDatastores where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFHIRDatastores where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFHIRDatastoresResponse' smart constructor.
data ListFHIRDatastoresResponse = ListFHIRDatastoresResponse'
  { -- | Pagination token that can be used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | All properties associated with the listed Data Stores.
    datastorePropertiesList :: [DatastoreProperties]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFHIRDatastoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFHIRDatastoresResponse_nextToken' - Pagination token that can be used to retrieve the next page of results.
--
-- 'httpStatus', 'listFHIRDatastoresResponse_httpStatus' - The response's http status code.
--
-- 'datastorePropertiesList', 'listFHIRDatastoresResponse_datastorePropertiesList' - All properties associated with the listed Data Stores.
newListFHIRDatastoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFHIRDatastoresResponse
newListFHIRDatastoresResponse pHttpStatus_ =
  ListFHIRDatastoresResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      datastorePropertiesList = Prelude.mempty
    }

-- | Pagination token that can be used to retrieve the next page of results.
listFHIRDatastoresResponse_nextToken :: Lens.Lens' ListFHIRDatastoresResponse (Prelude.Maybe Prelude.Text)
listFHIRDatastoresResponse_nextToken = Lens.lens (\ListFHIRDatastoresResponse' {nextToken} -> nextToken) (\s@ListFHIRDatastoresResponse' {} a -> s {nextToken = a} :: ListFHIRDatastoresResponse)

-- | The response's http status code.
listFHIRDatastoresResponse_httpStatus :: Lens.Lens' ListFHIRDatastoresResponse Prelude.Int
listFHIRDatastoresResponse_httpStatus = Lens.lens (\ListFHIRDatastoresResponse' {httpStatus} -> httpStatus) (\s@ListFHIRDatastoresResponse' {} a -> s {httpStatus = a} :: ListFHIRDatastoresResponse)

-- | All properties associated with the listed Data Stores.
listFHIRDatastoresResponse_datastorePropertiesList :: Lens.Lens' ListFHIRDatastoresResponse [DatastoreProperties]
listFHIRDatastoresResponse_datastorePropertiesList = Lens.lens (\ListFHIRDatastoresResponse' {datastorePropertiesList} -> datastorePropertiesList) (\s@ListFHIRDatastoresResponse' {} a -> s {datastorePropertiesList = a} :: ListFHIRDatastoresResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFHIRDatastoresResponse where
  rnf ListFHIRDatastoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf datastorePropertiesList
