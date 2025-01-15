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
-- Module      : Amazonka.AppIntegrationS.ListDataIntegrationAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of DataIntegration associations in the account.
--
-- You cannot create a DataIntegration association for a DataIntegration
-- that has been previously associated. Use a different DataIntegration, or
-- recreate the DataIntegration using the
-- <https://docs.aws.amazon.com/appintegrations/latest/APIReference/API_CreateDataIntegration.html CreateDataIntegration>
-- API.
module Amazonka.AppIntegrationS.ListDataIntegrationAssociations
  ( -- * Creating a Request
    ListDataIntegrationAssociations (..),
    newListDataIntegrationAssociations,

    -- * Request Lenses
    listDataIntegrationAssociations_maxResults,
    listDataIntegrationAssociations_nextToken,
    listDataIntegrationAssociations_dataIntegrationIdentifier,

    -- * Destructuring the Response
    ListDataIntegrationAssociationsResponse (..),
    newListDataIntegrationAssociationsResponse,

    -- * Response Lenses
    listDataIntegrationAssociationsResponse_dataIntegrationAssociations,
    listDataIntegrationAssociationsResponse_nextToken,
    listDataIntegrationAssociationsResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataIntegrationAssociations' smart constructor.
data ListDataIntegrationAssociations = ListDataIntegrationAssociations'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the DataIntegration.
    dataIntegrationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataIntegrationAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataIntegrationAssociations_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listDataIntegrationAssociations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'dataIntegrationIdentifier', 'listDataIntegrationAssociations_dataIntegrationIdentifier' - A unique identifier for the DataIntegration.
newListDataIntegrationAssociations ::
  -- | 'dataIntegrationIdentifier'
  Prelude.Text ->
  ListDataIntegrationAssociations
newListDataIntegrationAssociations
  pDataIntegrationIdentifier_ =
    ListDataIntegrationAssociations'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        dataIntegrationIdentifier =
          pDataIntegrationIdentifier_
      }

-- | The maximum number of results to return per page.
listDataIntegrationAssociations_maxResults :: Lens.Lens' ListDataIntegrationAssociations (Prelude.Maybe Prelude.Natural)
listDataIntegrationAssociations_maxResults = Lens.lens (\ListDataIntegrationAssociations' {maxResults} -> maxResults) (\s@ListDataIntegrationAssociations' {} a -> s {maxResults = a} :: ListDataIntegrationAssociations)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listDataIntegrationAssociations_nextToken :: Lens.Lens' ListDataIntegrationAssociations (Prelude.Maybe Prelude.Text)
listDataIntegrationAssociations_nextToken = Lens.lens (\ListDataIntegrationAssociations' {nextToken} -> nextToken) (\s@ListDataIntegrationAssociations' {} a -> s {nextToken = a} :: ListDataIntegrationAssociations)

-- | A unique identifier for the DataIntegration.
listDataIntegrationAssociations_dataIntegrationIdentifier :: Lens.Lens' ListDataIntegrationAssociations Prelude.Text
listDataIntegrationAssociations_dataIntegrationIdentifier = Lens.lens (\ListDataIntegrationAssociations' {dataIntegrationIdentifier} -> dataIntegrationIdentifier) (\s@ListDataIntegrationAssociations' {} a -> s {dataIntegrationIdentifier = a} :: ListDataIntegrationAssociations)

instance
  Core.AWSRequest
    ListDataIntegrationAssociations
  where
  type
    AWSResponse ListDataIntegrationAssociations =
      ListDataIntegrationAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataIntegrationAssociationsResponse'
            Prelude.<$> (x Data..?> "DataIntegrationAssociations")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDataIntegrationAssociations
  where
  hashWithSalt
    _salt
    ListDataIntegrationAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` dataIntegrationIdentifier

instance
  Prelude.NFData
    ListDataIntegrationAssociations
  where
  rnf ListDataIntegrationAssociations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf dataIntegrationIdentifier

instance
  Data.ToHeaders
    ListDataIntegrationAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataIntegrationAssociations where
  toPath ListDataIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "/dataIntegrations/",
        Data.toBS dataIntegrationIdentifier,
        "/associations"
      ]

instance Data.ToQuery ListDataIntegrationAssociations where
  toQuery ListDataIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDataIntegrationAssociationsResponse' smart constructor.
data ListDataIntegrationAssociationsResponse = ListDataIntegrationAssociationsResponse'
  { -- | The Amazon Resource Name (ARN) and unique ID of the DataIntegration
    -- association.
    dataIntegrationAssociations :: Prelude.Maybe (Prelude.NonEmpty DataIntegrationAssociationSummary),
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataIntegrationAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataIntegrationAssociations', 'listDataIntegrationAssociationsResponse_dataIntegrationAssociations' - The Amazon Resource Name (ARN) and unique ID of the DataIntegration
-- association.
--
-- 'nextToken', 'listDataIntegrationAssociationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listDataIntegrationAssociationsResponse_httpStatus' - The response's http status code.
newListDataIntegrationAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataIntegrationAssociationsResponse
newListDataIntegrationAssociationsResponse
  pHttpStatus_ =
    ListDataIntegrationAssociationsResponse'
      { dataIntegrationAssociations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) and unique ID of the DataIntegration
-- association.
listDataIntegrationAssociationsResponse_dataIntegrationAssociations :: Lens.Lens' ListDataIntegrationAssociationsResponse (Prelude.Maybe (Prelude.NonEmpty DataIntegrationAssociationSummary))
listDataIntegrationAssociationsResponse_dataIntegrationAssociations = Lens.lens (\ListDataIntegrationAssociationsResponse' {dataIntegrationAssociations} -> dataIntegrationAssociations) (\s@ListDataIntegrationAssociationsResponse' {} a -> s {dataIntegrationAssociations = a} :: ListDataIntegrationAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listDataIntegrationAssociationsResponse_nextToken :: Lens.Lens' ListDataIntegrationAssociationsResponse (Prelude.Maybe Prelude.Text)
listDataIntegrationAssociationsResponse_nextToken = Lens.lens (\ListDataIntegrationAssociationsResponse' {nextToken} -> nextToken) (\s@ListDataIntegrationAssociationsResponse' {} a -> s {nextToken = a} :: ListDataIntegrationAssociationsResponse)

-- | The response's http status code.
listDataIntegrationAssociationsResponse_httpStatus :: Lens.Lens' ListDataIntegrationAssociationsResponse Prelude.Int
listDataIntegrationAssociationsResponse_httpStatus = Lens.lens (\ListDataIntegrationAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListDataIntegrationAssociationsResponse' {} a -> s {httpStatus = a} :: ListDataIntegrationAssociationsResponse)

instance
  Prelude.NFData
    ListDataIntegrationAssociationsResponse
  where
  rnf ListDataIntegrationAssociationsResponse' {..} =
    Prelude.rnf dataIntegrationAssociations `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
