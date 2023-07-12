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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.ListUserAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists user associations for an identity provider.
--
-- This operation returns paginated results.
module Amazonka.LicenseManagerUserSubscriptions.ListUserAssociations
  ( -- * Creating a Request
    ListUserAssociations (..),
    newListUserAssociations,

    -- * Request Lenses
    listUserAssociations_filters,
    listUserAssociations_maxResults,
    listUserAssociations_nextToken,
    listUserAssociations_identityProvider,
    listUserAssociations_instanceId,

    -- * Destructuring the Response
    ListUserAssociationsResponse (..),
    newListUserAssociationsResponse,

    -- * Response Lenses
    listUserAssociationsResponse_instanceUserSummaries,
    listUserAssociationsResponse_nextToken,
    listUserAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUserAssociations' smart constructor.
data ListUserAssociations = ListUserAssociations'
  { -- | An array of structures that you can use to filter the results to those
    -- that match one or more sets of key-value pairs that you specify.
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The ID of the EC2 instance, which provides user-based subscriptions.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listUserAssociations_filters' - An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify.
--
-- 'maxResults', 'listUserAssociations_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listUserAssociations_nextToken' - Token for the next set of results.
--
-- 'identityProvider', 'listUserAssociations_identityProvider' - An object that specifies details for the identity provider.
--
-- 'instanceId', 'listUserAssociations_instanceId' - The ID of the EC2 instance, which provides user-based subscriptions.
newListUserAssociations ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'instanceId'
  Prelude.Text ->
  ListUserAssociations
newListUserAssociations
  pIdentityProvider_
  pInstanceId_ =
    ListUserAssociations'
      { filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        instanceId = pInstanceId_
      }

-- | An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify.
listUserAssociations_filters :: Lens.Lens' ListUserAssociations (Prelude.Maybe [Filter])
listUserAssociations_filters = Lens.lens (\ListUserAssociations' {filters} -> filters) (\s@ListUserAssociations' {} a -> s {filters = a} :: ListUserAssociations) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listUserAssociations_maxResults :: Lens.Lens' ListUserAssociations (Prelude.Maybe Prelude.Int)
listUserAssociations_maxResults = Lens.lens (\ListUserAssociations' {maxResults} -> maxResults) (\s@ListUserAssociations' {} a -> s {maxResults = a} :: ListUserAssociations)

-- | Token for the next set of results.
listUserAssociations_nextToken :: Lens.Lens' ListUserAssociations (Prelude.Maybe Prelude.Text)
listUserAssociations_nextToken = Lens.lens (\ListUserAssociations' {nextToken} -> nextToken) (\s@ListUserAssociations' {} a -> s {nextToken = a} :: ListUserAssociations)

-- | An object that specifies details for the identity provider.
listUserAssociations_identityProvider :: Lens.Lens' ListUserAssociations IdentityProvider
listUserAssociations_identityProvider = Lens.lens (\ListUserAssociations' {identityProvider} -> identityProvider) (\s@ListUserAssociations' {} a -> s {identityProvider = a} :: ListUserAssociations)

-- | The ID of the EC2 instance, which provides user-based subscriptions.
listUserAssociations_instanceId :: Lens.Lens' ListUserAssociations Prelude.Text
listUserAssociations_instanceId = Lens.lens (\ListUserAssociations' {instanceId} -> instanceId) (\s@ListUserAssociations' {} a -> s {instanceId = a} :: ListUserAssociations)

instance Core.AWSPager ListUserAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUserAssociationsResponse_instanceUserSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listUserAssociations_nextToken
          Lens..~ rs
          Lens.^? listUserAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListUserAssociations where
  type
    AWSResponse ListUserAssociations =
      ListUserAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserAssociationsResponse'
            Prelude.<$> ( x
                            Data..?> "InstanceUserSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUserAssociations where
  hashWithSalt _salt ListUserAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListUserAssociations where
  rnf ListUserAssociations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListUserAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListUserAssociations where
  toJSON ListUserAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath ListUserAssociations where
  toPath = Prelude.const "/user/ListUserAssociations"

instance Data.ToQuery ListUserAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListUserAssociationsResponse' smart constructor.
data ListUserAssociationsResponse = ListUserAssociationsResponse'
  { -- | Metadata that describes the list user association operation.
    instanceUserSummaries :: Prelude.Maybe [InstanceUserSummary],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUserSummaries', 'listUserAssociationsResponse_instanceUserSummaries' - Metadata that describes the list user association operation.
--
-- 'nextToken', 'listUserAssociationsResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listUserAssociationsResponse_httpStatus' - The response's http status code.
newListUserAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserAssociationsResponse
newListUserAssociationsResponse pHttpStatus_ =
  ListUserAssociationsResponse'
    { instanceUserSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata that describes the list user association operation.
listUserAssociationsResponse_instanceUserSummaries :: Lens.Lens' ListUserAssociationsResponse (Prelude.Maybe [InstanceUserSummary])
listUserAssociationsResponse_instanceUserSummaries = Lens.lens (\ListUserAssociationsResponse' {instanceUserSummaries} -> instanceUserSummaries) (\s@ListUserAssociationsResponse' {} a -> s {instanceUserSummaries = a} :: ListUserAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listUserAssociationsResponse_nextToken :: Lens.Lens' ListUserAssociationsResponse (Prelude.Maybe Prelude.Text)
listUserAssociationsResponse_nextToken = Lens.lens (\ListUserAssociationsResponse' {nextToken} -> nextToken) (\s@ListUserAssociationsResponse' {} a -> s {nextToken = a} :: ListUserAssociationsResponse)

-- | The response's http status code.
listUserAssociationsResponse_httpStatus :: Lens.Lens' ListUserAssociationsResponse Prelude.Int
listUserAssociationsResponse_httpStatus = Lens.lens (\ListUserAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListUserAssociationsResponse' {} a -> s {httpStatus = a} :: ListUserAssociationsResponse)

instance Prelude.NFData ListUserAssociationsResponse where
  rnf ListUserAssociationsResponse' {..} =
    Prelude.rnf instanceUserSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
