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
-- Module      : Amazonka.Transfer.ListAgreements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the agreements for the server that\'s identified by
-- the @ServerId@ that you supply. If you want to limit the results to a
-- certain number, supply a value for the @MaxResults@ parameter. If you
-- ran the command previously and received a value for @NextToken@, you can
-- supply that value to continue listing agreements from where you left
-- off.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListAgreements
  ( -- * Creating a Request
    ListAgreements (..),
    newListAgreements,

    -- * Request Lenses
    listAgreements_nextToken,
    listAgreements_maxResults,
    listAgreements_serverId,

    -- * Destructuring the Response
    ListAgreementsResponse (..),
    newListAgreementsResponse,

    -- * Response Lenses
    listAgreementsResponse_nextToken,
    listAgreementsResponse_httpStatus,
    listAgreementsResponse_agreements,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListAgreements' smart constructor.
data ListAgreements = ListAgreements'
  { -- | When you can get additional results from the @ListAgreements@ call, a
    -- @NextToken@ parameter is returned in the output. You can then pass in a
    -- subsequent command to the @NextToken@ parameter to continue listing
    -- additional agreements.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of agreements to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the server for which you want a list of agreements.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAgreements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAgreements_nextToken' - When you can get additional results from the @ListAgreements@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional agreements.
--
-- 'maxResults', 'listAgreements_maxResults' - The maximum number of agreements to return.
--
-- 'serverId', 'listAgreements_serverId' - The identifier of the server for which you want a list of agreements.
newListAgreements ::
  -- | 'serverId'
  Prelude.Text ->
  ListAgreements
newListAgreements pServerId_ =
  ListAgreements'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      serverId = pServerId_
    }

-- | When you can get additional results from the @ListAgreements@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional agreements.
listAgreements_nextToken :: Lens.Lens' ListAgreements (Prelude.Maybe Prelude.Text)
listAgreements_nextToken = Lens.lens (\ListAgreements' {nextToken} -> nextToken) (\s@ListAgreements' {} a -> s {nextToken = a} :: ListAgreements)

-- | The maximum number of agreements to return.
listAgreements_maxResults :: Lens.Lens' ListAgreements (Prelude.Maybe Prelude.Natural)
listAgreements_maxResults = Lens.lens (\ListAgreements' {maxResults} -> maxResults) (\s@ListAgreements' {} a -> s {maxResults = a} :: ListAgreements)

-- | The identifier of the server for which you want a list of agreements.
listAgreements_serverId :: Lens.Lens' ListAgreements Prelude.Text
listAgreements_serverId = Lens.lens (\ListAgreements' {serverId} -> serverId) (\s@ListAgreements' {} a -> s {serverId = a} :: ListAgreements)

instance Core.AWSPager ListAgreements where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAgreementsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listAgreementsResponse_agreements) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAgreements_nextToken
          Lens..~ rs
          Lens.^? listAgreementsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAgreements where
  type
    AWSResponse ListAgreements =
      ListAgreementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAgreementsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Agreements" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListAgreements where
  hashWithSalt _salt ListAgreements' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData ListAgreements where
  rnf ListAgreements' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf serverId

instance Data.ToHeaders ListAgreements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListAgreements" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAgreements where
  toJSON ListAgreements' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("ServerId" Data..= serverId)
          ]
      )

instance Data.ToPath ListAgreements where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAgreements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAgreementsResponse' smart constructor.
data ListAgreementsResponse = ListAgreementsResponse'
  { -- | Returns a token that you can use to call @ListAgreements@ again and
    -- receive additional results, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns an array, where each item contains the details of an agreement.
    agreements :: [ListedAgreement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAgreementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAgreementsResponse_nextToken' - Returns a token that you can use to call @ListAgreements@ again and
-- receive additional results, if there are any.
--
-- 'httpStatus', 'listAgreementsResponse_httpStatus' - The response's http status code.
--
-- 'agreements', 'listAgreementsResponse_agreements' - Returns an array, where each item contains the details of an agreement.
newListAgreementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAgreementsResponse
newListAgreementsResponse pHttpStatus_ =
  ListAgreementsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      agreements = Prelude.mempty
    }

-- | Returns a token that you can use to call @ListAgreements@ again and
-- receive additional results, if there are any.
listAgreementsResponse_nextToken :: Lens.Lens' ListAgreementsResponse (Prelude.Maybe Prelude.Text)
listAgreementsResponse_nextToken = Lens.lens (\ListAgreementsResponse' {nextToken} -> nextToken) (\s@ListAgreementsResponse' {} a -> s {nextToken = a} :: ListAgreementsResponse)

-- | The response's http status code.
listAgreementsResponse_httpStatus :: Lens.Lens' ListAgreementsResponse Prelude.Int
listAgreementsResponse_httpStatus = Lens.lens (\ListAgreementsResponse' {httpStatus} -> httpStatus) (\s@ListAgreementsResponse' {} a -> s {httpStatus = a} :: ListAgreementsResponse)

-- | Returns an array, where each item contains the details of an agreement.
listAgreementsResponse_agreements :: Lens.Lens' ListAgreementsResponse [ListedAgreement]
listAgreementsResponse_agreements = Lens.lens (\ListAgreementsResponse' {agreements} -> agreements) (\s@ListAgreementsResponse' {} a -> s {agreements = a} :: ListAgreementsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAgreementsResponse where
  rnf ListAgreementsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf agreements
