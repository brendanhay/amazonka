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
-- Module      : Amazonka.PinpointSmsVoiceV2.ListPoolOriginationIdentities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all associated origination identities in your pool.
--
-- If you specify filters, the output includes information for only those
-- origination identities that meet the filter criteria.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.ListPoolOriginationIdentities
  ( -- * Creating a Request
    ListPoolOriginationIdentities (..),
    newListPoolOriginationIdentities,

    -- * Request Lenses
    listPoolOriginationIdentities_filters,
    listPoolOriginationIdentities_maxResults,
    listPoolOriginationIdentities_nextToken,
    listPoolOriginationIdentities_poolId,

    -- * Destructuring the Response
    ListPoolOriginationIdentitiesResponse (..),
    newListPoolOriginationIdentitiesResponse,

    -- * Response Lenses
    listPoolOriginationIdentitiesResponse_nextToken,
    listPoolOriginationIdentitiesResponse_originationIdentities,
    listPoolOriginationIdentitiesResponse_poolArn,
    listPoolOriginationIdentitiesResponse_poolId,
    listPoolOriginationIdentitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPoolOriginationIdentities' smart constructor.
data ListPoolOriginationIdentities = ListPoolOriginationIdentities'
  { -- | An array of PoolOriginationIdentitiesFilter objects to filter the
    -- results..
    filters :: Prelude.Maybe [PoolOriginationIdentitiesFilter],
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the pool. This value can be either the PoolId
    -- or PoolArn.
    poolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPoolOriginationIdentities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listPoolOriginationIdentities_filters' - An array of PoolOriginationIdentitiesFilter objects to filter the
-- results..
--
-- 'maxResults', 'listPoolOriginationIdentities_maxResults' - The maximum number of results to return per each request.
--
-- 'nextToken', 'listPoolOriginationIdentities_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'poolId', 'listPoolOriginationIdentities_poolId' - The unique identifier for the pool. This value can be either the PoolId
-- or PoolArn.
newListPoolOriginationIdentities ::
  -- | 'poolId'
  Prelude.Text ->
  ListPoolOriginationIdentities
newListPoolOriginationIdentities pPoolId_ =
  ListPoolOriginationIdentities'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      poolId = pPoolId_
    }

-- | An array of PoolOriginationIdentitiesFilter objects to filter the
-- results..
listPoolOriginationIdentities_filters :: Lens.Lens' ListPoolOriginationIdentities (Prelude.Maybe [PoolOriginationIdentitiesFilter])
listPoolOriginationIdentities_filters = Lens.lens (\ListPoolOriginationIdentities' {filters} -> filters) (\s@ListPoolOriginationIdentities' {} a -> s {filters = a} :: ListPoolOriginationIdentities) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per each request.
listPoolOriginationIdentities_maxResults :: Lens.Lens' ListPoolOriginationIdentities (Prelude.Maybe Prelude.Natural)
listPoolOriginationIdentities_maxResults = Lens.lens (\ListPoolOriginationIdentities' {maxResults} -> maxResults) (\s@ListPoolOriginationIdentities' {} a -> s {maxResults = a} :: ListPoolOriginationIdentities)

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
listPoolOriginationIdentities_nextToken :: Lens.Lens' ListPoolOriginationIdentities (Prelude.Maybe Prelude.Text)
listPoolOriginationIdentities_nextToken = Lens.lens (\ListPoolOriginationIdentities' {nextToken} -> nextToken) (\s@ListPoolOriginationIdentities' {} a -> s {nextToken = a} :: ListPoolOriginationIdentities)

-- | The unique identifier for the pool. This value can be either the PoolId
-- or PoolArn.
listPoolOriginationIdentities_poolId :: Lens.Lens' ListPoolOriginationIdentities Prelude.Text
listPoolOriginationIdentities_poolId = Lens.lens (\ListPoolOriginationIdentities' {poolId} -> poolId) (\s@ListPoolOriginationIdentities' {} a -> s {poolId = a} :: ListPoolOriginationIdentities)

instance Core.AWSPager ListPoolOriginationIdentities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPoolOriginationIdentitiesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPoolOriginationIdentitiesResponse_originationIdentities
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPoolOriginationIdentities_nextToken
          Lens..~ rs
          Lens.^? listPoolOriginationIdentitiesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListPoolOriginationIdentities
  where
  type
    AWSResponse ListPoolOriginationIdentities =
      ListPoolOriginationIdentitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoolOriginationIdentitiesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "OriginationIdentities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "PoolArn")
            Prelude.<*> (x Data..?> "PoolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPoolOriginationIdentities
  where
  hashWithSalt _salt ListPoolOriginationIdentities' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` poolId

instance Prelude.NFData ListPoolOriginationIdentities where
  rnf ListPoolOriginationIdentities' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf poolId

instance Data.ToHeaders ListPoolOriginationIdentities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.ListPoolOriginationIdentities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPoolOriginationIdentities where
  toJSON ListPoolOriginationIdentities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("PoolId" Data..= poolId)
          ]
      )

instance Data.ToPath ListPoolOriginationIdentities where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPoolOriginationIdentities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPoolOriginationIdentitiesResponse' smart constructor.
data ListPoolOriginationIdentitiesResponse = ListPoolOriginationIdentitiesResponse'
  { -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of any OriginationIdentityMetadata objects.
    originationIdentities :: Prelude.Maybe [OriginationIdentityMetadata],
    -- | The Amazon Resource Name (ARN) for the pool.
    poolArn :: Prelude.Maybe Prelude.Text,
    -- | The unique PoolId of the pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPoolOriginationIdentitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPoolOriginationIdentitiesResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'originationIdentities', 'listPoolOriginationIdentitiesResponse_originationIdentities' - An array of any OriginationIdentityMetadata objects.
--
-- 'poolArn', 'listPoolOriginationIdentitiesResponse_poolArn' - The Amazon Resource Name (ARN) for the pool.
--
-- 'poolId', 'listPoolOriginationIdentitiesResponse_poolId' - The unique PoolId of the pool.
--
-- 'httpStatus', 'listPoolOriginationIdentitiesResponse_httpStatus' - The response's http status code.
newListPoolOriginationIdentitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPoolOriginationIdentitiesResponse
newListPoolOriginationIdentitiesResponse pHttpStatus_ =
  ListPoolOriginationIdentitiesResponse'
    { nextToken =
        Prelude.Nothing,
      originationIdentities =
        Prelude.Nothing,
      poolArn = Prelude.Nothing,
      poolId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
listPoolOriginationIdentitiesResponse_nextToken :: Lens.Lens' ListPoolOriginationIdentitiesResponse (Prelude.Maybe Prelude.Text)
listPoolOriginationIdentitiesResponse_nextToken = Lens.lens (\ListPoolOriginationIdentitiesResponse' {nextToken} -> nextToken) (\s@ListPoolOriginationIdentitiesResponse' {} a -> s {nextToken = a} :: ListPoolOriginationIdentitiesResponse)

-- | An array of any OriginationIdentityMetadata objects.
listPoolOriginationIdentitiesResponse_originationIdentities :: Lens.Lens' ListPoolOriginationIdentitiesResponse (Prelude.Maybe [OriginationIdentityMetadata])
listPoolOriginationIdentitiesResponse_originationIdentities = Lens.lens (\ListPoolOriginationIdentitiesResponse' {originationIdentities} -> originationIdentities) (\s@ListPoolOriginationIdentitiesResponse' {} a -> s {originationIdentities = a} :: ListPoolOriginationIdentitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the pool.
listPoolOriginationIdentitiesResponse_poolArn :: Lens.Lens' ListPoolOriginationIdentitiesResponse (Prelude.Maybe Prelude.Text)
listPoolOriginationIdentitiesResponse_poolArn = Lens.lens (\ListPoolOriginationIdentitiesResponse' {poolArn} -> poolArn) (\s@ListPoolOriginationIdentitiesResponse' {} a -> s {poolArn = a} :: ListPoolOriginationIdentitiesResponse)

-- | The unique PoolId of the pool.
listPoolOriginationIdentitiesResponse_poolId :: Lens.Lens' ListPoolOriginationIdentitiesResponse (Prelude.Maybe Prelude.Text)
listPoolOriginationIdentitiesResponse_poolId = Lens.lens (\ListPoolOriginationIdentitiesResponse' {poolId} -> poolId) (\s@ListPoolOriginationIdentitiesResponse' {} a -> s {poolId = a} :: ListPoolOriginationIdentitiesResponse)

-- | The response's http status code.
listPoolOriginationIdentitiesResponse_httpStatus :: Lens.Lens' ListPoolOriginationIdentitiesResponse Prelude.Int
listPoolOriginationIdentitiesResponse_httpStatus = Lens.lens (\ListPoolOriginationIdentitiesResponse' {httpStatus} -> httpStatus) (\s@ListPoolOriginationIdentitiesResponse' {} a -> s {httpStatus = a} :: ListPoolOriginationIdentitiesResponse)

instance
  Prelude.NFData
    ListPoolOriginationIdentitiesResponse
  where
  rnf ListPoolOriginationIdentitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf originationIdentities
      `Prelude.seq` Prelude.rnf poolArn
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf httpStatus
