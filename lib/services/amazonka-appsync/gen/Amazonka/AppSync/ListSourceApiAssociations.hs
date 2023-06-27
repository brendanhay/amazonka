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
-- Module      : Amazonka.AppSync.ListSourceApiAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the @SourceApiAssociationSummary@ data.
module Amazonka.AppSync.ListSourceApiAssociations
  ( -- * Creating a Request
    ListSourceApiAssociations (..),
    newListSourceApiAssociations,

    -- * Request Lenses
    listSourceApiAssociations_maxResults,
    listSourceApiAssociations_nextToken,
    listSourceApiAssociations_apiId,

    -- * Destructuring the Response
    ListSourceApiAssociationsResponse (..),
    newListSourceApiAssociationsResponse,

    -- * Response Lenses
    listSourceApiAssociationsResponse_nextToken,
    listSourceApiAssociationsResponse_sourceApiAssociationSummaries,
    listSourceApiAssociationsResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSourceApiAssociations' smart constructor.
data ListSourceApiAssociations = ListSourceApiAssociations'
  { -- | The maximum number of results that you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceApiAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSourceApiAssociations_maxResults' - The maximum number of results that you want the request to return.
--
-- 'nextToken', 'listSourceApiAssociations_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
--
-- 'apiId', 'listSourceApiAssociations_apiId' - The API ID.
newListSourceApiAssociations ::
  -- | 'apiId'
  Prelude.Text ->
  ListSourceApiAssociations
newListSourceApiAssociations pApiId_ =
  ListSourceApiAssociations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The maximum number of results that you want the request to return.
listSourceApiAssociations_maxResults :: Lens.Lens' ListSourceApiAssociations (Prelude.Maybe Prelude.Natural)
listSourceApiAssociations_maxResults = Lens.lens (\ListSourceApiAssociations' {maxResults} -> maxResults) (\s@ListSourceApiAssociations' {} a -> s {maxResults = a} :: ListSourceApiAssociations)

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listSourceApiAssociations_nextToken :: Lens.Lens' ListSourceApiAssociations (Prelude.Maybe Prelude.Text)
listSourceApiAssociations_nextToken = Lens.lens (\ListSourceApiAssociations' {nextToken} -> nextToken) (\s@ListSourceApiAssociations' {} a -> s {nextToken = a} :: ListSourceApiAssociations)

-- | The API ID.
listSourceApiAssociations_apiId :: Lens.Lens' ListSourceApiAssociations Prelude.Text
listSourceApiAssociations_apiId = Lens.lens (\ListSourceApiAssociations' {apiId} -> apiId) (\s@ListSourceApiAssociations' {} a -> s {apiId = a} :: ListSourceApiAssociations)

instance Core.AWSRequest ListSourceApiAssociations where
  type
    AWSResponse ListSourceApiAssociations =
      ListSourceApiAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSourceApiAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "sourceApiAssociationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSourceApiAssociations where
  hashWithSalt _salt ListSourceApiAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData ListSourceApiAssociations where
  rnf ListSourceApiAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders ListSourceApiAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSourceApiAssociations where
  toPath ListSourceApiAssociations' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/sourceApiAssociations"
      ]

instance Data.ToQuery ListSourceApiAssociations where
  toQuery ListSourceApiAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSourceApiAssociationsResponse' smart constructor.
data ListSourceApiAssociationsResponse = ListSourceApiAssociationsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The @SourceApiAssociationSummary@ object data.
    sourceApiAssociationSummaries :: Prelude.Maybe [SourceApiAssociationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceApiAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSourceApiAssociationsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
--
-- 'sourceApiAssociationSummaries', 'listSourceApiAssociationsResponse_sourceApiAssociationSummaries' - The @SourceApiAssociationSummary@ object data.
--
-- 'httpStatus', 'listSourceApiAssociationsResponse_httpStatus' - The response's http status code.
newListSourceApiAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSourceApiAssociationsResponse
newListSourceApiAssociationsResponse pHttpStatus_ =
  ListSourceApiAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      sourceApiAssociationSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listSourceApiAssociationsResponse_nextToken :: Lens.Lens' ListSourceApiAssociationsResponse (Prelude.Maybe Prelude.Text)
listSourceApiAssociationsResponse_nextToken = Lens.lens (\ListSourceApiAssociationsResponse' {nextToken} -> nextToken) (\s@ListSourceApiAssociationsResponse' {} a -> s {nextToken = a} :: ListSourceApiAssociationsResponse)

-- | The @SourceApiAssociationSummary@ object data.
listSourceApiAssociationsResponse_sourceApiAssociationSummaries :: Lens.Lens' ListSourceApiAssociationsResponse (Prelude.Maybe [SourceApiAssociationSummary])
listSourceApiAssociationsResponse_sourceApiAssociationSummaries = Lens.lens (\ListSourceApiAssociationsResponse' {sourceApiAssociationSummaries} -> sourceApiAssociationSummaries) (\s@ListSourceApiAssociationsResponse' {} a -> s {sourceApiAssociationSummaries = a} :: ListSourceApiAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSourceApiAssociationsResponse_httpStatus :: Lens.Lens' ListSourceApiAssociationsResponse Prelude.Int
listSourceApiAssociationsResponse_httpStatus = Lens.lens (\ListSourceApiAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListSourceApiAssociationsResponse' {} a -> s {httpStatus = a} :: ListSourceApiAssociationsResponse)

instance
  Prelude.NFData
    ListSourceApiAssociationsResponse
  where
  rnf ListSourceApiAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sourceApiAssociationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
