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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listDataIntegrationAssociations_nextToken,
    listDataIntegrationAssociations_maxResults,
    listDataIntegrationAssociations_dataIntegrationIdentifier,

    -- * Destructuring the Response
    ListDataIntegrationAssociationsResponse (..),
    newListDataIntegrationAssociationsResponse,

    -- * Response Lenses
    listDataIntegrationAssociationsResponse_nextToken,
    listDataIntegrationAssociationsResponse_dataIntegrationAssociations,
    listDataIntegrationAssociationsResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataIntegrationAssociations' smart constructor.
data ListDataIntegrationAssociations = ListDataIntegrationAssociations'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listDataIntegrationAssociations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listDataIntegrationAssociations_maxResults' - The maximum number of results to return per page.
--
-- 'dataIntegrationIdentifier', 'listDataIntegrationAssociations_dataIntegrationIdentifier' - A unique identifier for the DataIntegration.
newListDataIntegrationAssociations ::
  -- | 'dataIntegrationIdentifier'
  Prelude.Text ->
  ListDataIntegrationAssociations
newListDataIntegrationAssociations
  pDataIntegrationIdentifier_ =
    ListDataIntegrationAssociations'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        dataIntegrationIdentifier =
          pDataIntegrationIdentifier_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listDataIntegrationAssociations_nextToken :: Lens.Lens' ListDataIntegrationAssociations (Prelude.Maybe Prelude.Text)
listDataIntegrationAssociations_nextToken = Lens.lens (\ListDataIntegrationAssociations' {nextToken} -> nextToken) (\s@ListDataIntegrationAssociations' {} a -> s {nextToken = a} :: ListDataIntegrationAssociations)

-- | The maximum number of results to return per page.
listDataIntegrationAssociations_maxResults :: Lens.Lens' ListDataIntegrationAssociations (Prelude.Maybe Prelude.Natural)
listDataIntegrationAssociations_maxResults = Lens.lens (\ListDataIntegrationAssociations' {maxResults} -> maxResults) (\s@ListDataIntegrationAssociations' {} a -> s {maxResults = a} :: ListDataIntegrationAssociations)

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "DataIntegrationAssociations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDataIntegrationAssociations
  where
  hashWithSalt
    _salt
    ListDataIntegrationAssociations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` dataIntegrationIdentifier

instance
  Prelude.NFData
    ListDataIntegrationAssociations
  where
  rnf ListDataIntegrationAssociations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dataIntegrationIdentifier

instance
  Core.ToHeaders
    ListDataIntegrationAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListDataIntegrationAssociations where
  toPath ListDataIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "/dataIntegrations/",
        Core.toBS dataIntegrationIdentifier,
        "/associations"
      ]

instance Core.ToQuery ListDataIntegrationAssociations where
  toQuery ListDataIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDataIntegrationAssociationsResponse' smart constructor.
data ListDataIntegrationAssociationsResponse = ListDataIntegrationAssociationsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) and unique ID of the DataIntegration
    -- association.
    dataIntegrationAssociations :: Prelude.Maybe (Prelude.NonEmpty DataIntegrationAssociationSummary),
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
-- 'nextToken', 'listDataIntegrationAssociationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'dataIntegrationAssociations', 'listDataIntegrationAssociationsResponse_dataIntegrationAssociations' - The Amazon Resource Name (ARN) and unique ID of the DataIntegration
-- association.
--
-- 'httpStatus', 'listDataIntegrationAssociationsResponse_httpStatus' - The response's http status code.
newListDataIntegrationAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataIntegrationAssociationsResponse
newListDataIntegrationAssociationsResponse
  pHttpStatus_ =
    ListDataIntegrationAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        dataIntegrationAssociations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If there are additional results, this is the token for the next set of
-- results.
listDataIntegrationAssociationsResponse_nextToken :: Lens.Lens' ListDataIntegrationAssociationsResponse (Prelude.Maybe Prelude.Text)
listDataIntegrationAssociationsResponse_nextToken = Lens.lens (\ListDataIntegrationAssociationsResponse' {nextToken} -> nextToken) (\s@ListDataIntegrationAssociationsResponse' {} a -> s {nextToken = a} :: ListDataIntegrationAssociationsResponse)

-- | The Amazon Resource Name (ARN) and unique ID of the DataIntegration
-- association.
listDataIntegrationAssociationsResponse_dataIntegrationAssociations :: Lens.Lens' ListDataIntegrationAssociationsResponse (Prelude.Maybe (Prelude.NonEmpty DataIntegrationAssociationSummary))
listDataIntegrationAssociationsResponse_dataIntegrationAssociations = Lens.lens (\ListDataIntegrationAssociationsResponse' {dataIntegrationAssociations} -> dataIntegrationAssociations) (\s@ListDataIntegrationAssociationsResponse' {} a -> s {dataIntegrationAssociations = a} :: ListDataIntegrationAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataIntegrationAssociationsResponse_httpStatus :: Lens.Lens' ListDataIntegrationAssociationsResponse Prelude.Int
listDataIntegrationAssociationsResponse_httpStatus = Lens.lens (\ListDataIntegrationAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListDataIntegrationAssociationsResponse' {} a -> s {httpStatus = a} :: ListDataIntegrationAssociationsResponse)

instance
  Prelude.NFData
    ListDataIntegrationAssociationsResponse
  where
  rnf ListDataIntegrationAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dataIntegrationAssociations
      `Prelude.seq` Prelude.rnf httpStatus
