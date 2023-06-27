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
-- Module      : Amazonka.CleanRooms.ListConfiguredTableAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists configured table associations for a membership.
--
-- This operation returns paginated results.
module Amazonka.CleanRooms.ListConfiguredTableAssociations
  ( -- * Creating a Request
    ListConfiguredTableAssociations (..),
    newListConfiguredTableAssociations,

    -- * Request Lenses
    listConfiguredTableAssociations_maxResults,
    listConfiguredTableAssociations_nextToken,
    listConfiguredTableAssociations_membershipIdentifier,

    -- * Destructuring the Response
    ListConfiguredTableAssociationsResponse (..),
    newListConfiguredTableAssociationsResponse,

    -- * Response Lenses
    listConfiguredTableAssociationsResponse_nextToken,
    listConfiguredTableAssociationsResponse_httpStatus,
    listConfiguredTableAssociationsResponse_configuredTableAssociationSummaries,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConfiguredTableAssociations' smart constructor.
data ListConfiguredTableAssociations = ListConfiguredTableAssociations'
  { -- | The maximum size of the results that is returned per call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the membership to list configured table
    -- associations for. Currently accepts the membership ID.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfiguredTableAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listConfiguredTableAssociations_maxResults' - The maximum size of the results that is returned per call.
--
-- 'nextToken', 'listConfiguredTableAssociations_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'membershipIdentifier', 'listConfiguredTableAssociations_membershipIdentifier' - A unique identifier for the membership to list configured table
-- associations for. Currently accepts the membership ID.
newListConfiguredTableAssociations ::
  -- | 'membershipIdentifier'
  Prelude.Text ->
  ListConfiguredTableAssociations
newListConfiguredTableAssociations
  pMembershipIdentifier_ =
    ListConfiguredTableAssociations'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        membershipIdentifier =
          pMembershipIdentifier_
      }

-- | The maximum size of the results that is returned per call.
listConfiguredTableAssociations_maxResults :: Lens.Lens' ListConfiguredTableAssociations (Prelude.Maybe Prelude.Natural)
listConfiguredTableAssociations_maxResults = Lens.lens (\ListConfiguredTableAssociations' {maxResults} -> maxResults) (\s@ListConfiguredTableAssociations' {} a -> s {maxResults = a} :: ListConfiguredTableAssociations)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listConfiguredTableAssociations_nextToken :: Lens.Lens' ListConfiguredTableAssociations (Prelude.Maybe Prelude.Text)
listConfiguredTableAssociations_nextToken = Lens.lens (\ListConfiguredTableAssociations' {nextToken} -> nextToken) (\s@ListConfiguredTableAssociations' {} a -> s {nextToken = a} :: ListConfiguredTableAssociations)

-- | A unique identifier for the membership to list configured table
-- associations for. Currently accepts the membership ID.
listConfiguredTableAssociations_membershipIdentifier :: Lens.Lens' ListConfiguredTableAssociations Prelude.Text
listConfiguredTableAssociations_membershipIdentifier = Lens.lens (\ListConfiguredTableAssociations' {membershipIdentifier} -> membershipIdentifier) (\s@ListConfiguredTableAssociations' {} a -> s {membershipIdentifier = a} :: ListConfiguredTableAssociations)

instance
  Core.AWSPager
    ListConfiguredTableAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfiguredTableAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listConfiguredTableAssociationsResponse_configuredTableAssociationSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listConfiguredTableAssociations_nextToken
          Lens..~ rs
          Lens.^? listConfiguredTableAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListConfiguredTableAssociations
  where
  type
    AWSResponse ListConfiguredTableAssociations =
      ListConfiguredTableAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfiguredTableAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "configuredTableAssociationSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListConfiguredTableAssociations
  where
  hashWithSalt
    _salt
    ListConfiguredTableAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` membershipIdentifier

instance
  Prelude.NFData
    ListConfiguredTableAssociations
  where
  rnf ListConfiguredTableAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf membershipIdentifier

instance
  Data.ToHeaders
    ListConfiguredTableAssociations
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

instance Data.ToPath ListConfiguredTableAssociations where
  toPath ListConfiguredTableAssociations' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/configuredTableAssociations"
      ]

instance Data.ToQuery ListConfiguredTableAssociations where
  toQuery ListConfiguredTableAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListConfiguredTableAssociationsResponse' smart constructor.
data ListConfiguredTableAssociationsResponse = ListConfiguredTableAssociationsResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The retrieved list of configured table associations.
    configuredTableAssociationSummaries :: [ConfiguredTableAssociationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfiguredTableAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfiguredTableAssociationsResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'httpStatus', 'listConfiguredTableAssociationsResponse_httpStatus' - The response's http status code.
--
-- 'configuredTableAssociationSummaries', 'listConfiguredTableAssociationsResponse_configuredTableAssociationSummaries' - The retrieved list of configured table associations.
newListConfiguredTableAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfiguredTableAssociationsResponse
newListConfiguredTableAssociationsResponse
  pHttpStatus_ =
    ListConfiguredTableAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        configuredTableAssociationSummaries =
          Prelude.mempty
      }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listConfiguredTableAssociationsResponse_nextToken :: Lens.Lens' ListConfiguredTableAssociationsResponse (Prelude.Maybe Prelude.Text)
listConfiguredTableAssociationsResponse_nextToken = Lens.lens (\ListConfiguredTableAssociationsResponse' {nextToken} -> nextToken) (\s@ListConfiguredTableAssociationsResponse' {} a -> s {nextToken = a} :: ListConfiguredTableAssociationsResponse)

-- | The response's http status code.
listConfiguredTableAssociationsResponse_httpStatus :: Lens.Lens' ListConfiguredTableAssociationsResponse Prelude.Int
listConfiguredTableAssociationsResponse_httpStatus = Lens.lens (\ListConfiguredTableAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListConfiguredTableAssociationsResponse' {} a -> s {httpStatus = a} :: ListConfiguredTableAssociationsResponse)

-- | The retrieved list of configured table associations.
listConfiguredTableAssociationsResponse_configuredTableAssociationSummaries :: Lens.Lens' ListConfiguredTableAssociationsResponse [ConfiguredTableAssociationSummary]
listConfiguredTableAssociationsResponse_configuredTableAssociationSummaries = Lens.lens (\ListConfiguredTableAssociationsResponse' {configuredTableAssociationSummaries} -> configuredTableAssociationSummaries) (\s@ListConfiguredTableAssociationsResponse' {} a -> s {configuredTableAssociationSummaries = a} :: ListConfiguredTableAssociationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListConfiguredTableAssociationsResponse
  where
  rnf ListConfiguredTableAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTableAssociationSummaries
