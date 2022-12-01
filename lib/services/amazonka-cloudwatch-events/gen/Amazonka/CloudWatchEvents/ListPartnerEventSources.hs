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
-- Module      : Amazonka.CloudWatchEvents.ListPartnerEventSources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list all the partner event
-- source names that they have created. This operation is not used by
-- Amazon Web Services customers.
module Amazonka.CloudWatchEvents.ListPartnerEventSources
  ( -- * Creating a Request
    ListPartnerEventSources (..),
    newListPartnerEventSources,

    -- * Request Lenses
    listPartnerEventSources_nextToken,
    listPartnerEventSources_limit,
    listPartnerEventSources_namePrefix,

    -- * Destructuring the Response
    ListPartnerEventSourcesResponse (..),
    newListPartnerEventSourcesResponse,

    -- * Response Lenses
    listPartnerEventSourcesResponse_nextToken,
    listPartnerEventSourcesResponse_partnerEventSources,
    listPartnerEventSourcesResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPartnerEventSources' smart constructor.
data ListPartnerEventSources = ListPartnerEventSources'
  { -- | The token returned by a previous call to this operation. Specifying this
    -- retrieves the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | pecifying this limits the number of results returned by this operation.
    -- The operation also returns a NextToken which you can use in a subsequent
    -- operation to retrieve the next set of results.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify this, the results are limited to only those partner event
    -- sources that start with the string you specify.
    namePrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPartnerEventSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPartnerEventSources_nextToken' - The token returned by a previous call to this operation. Specifying this
-- retrieves the next set of results.
--
-- 'limit', 'listPartnerEventSources_limit' - pecifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
--
-- 'namePrefix', 'listPartnerEventSources_namePrefix' - If you specify this, the results are limited to only those partner event
-- sources that start with the string you specify.
newListPartnerEventSources ::
  -- | 'namePrefix'
  Prelude.Text ->
  ListPartnerEventSources
newListPartnerEventSources pNamePrefix_ =
  ListPartnerEventSources'
    { nextToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      namePrefix = pNamePrefix_
    }

-- | The token returned by a previous call to this operation. Specifying this
-- retrieves the next set of results.
listPartnerEventSources_nextToken :: Lens.Lens' ListPartnerEventSources (Prelude.Maybe Prelude.Text)
listPartnerEventSources_nextToken = Lens.lens (\ListPartnerEventSources' {nextToken} -> nextToken) (\s@ListPartnerEventSources' {} a -> s {nextToken = a} :: ListPartnerEventSources)

-- | pecifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
listPartnerEventSources_limit :: Lens.Lens' ListPartnerEventSources (Prelude.Maybe Prelude.Natural)
listPartnerEventSources_limit = Lens.lens (\ListPartnerEventSources' {limit} -> limit) (\s@ListPartnerEventSources' {} a -> s {limit = a} :: ListPartnerEventSources)

-- | If you specify this, the results are limited to only those partner event
-- sources that start with the string you specify.
listPartnerEventSources_namePrefix :: Lens.Lens' ListPartnerEventSources Prelude.Text
listPartnerEventSources_namePrefix = Lens.lens (\ListPartnerEventSources' {namePrefix} -> namePrefix) (\s@ListPartnerEventSources' {} a -> s {namePrefix = a} :: ListPartnerEventSources)

instance Core.AWSRequest ListPartnerEventSources where
  type
    AWSResponse ListPartnerEventSources =
      ListPartnerEventSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPartnerEventSourcesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "PartnerEventSources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPartnerEventSources where
  hashWithSalt _salt ListPartnerEventSources' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` namePrefix

instance Prelude.NFData ListPartnerEventSources where
  rnf ListPartnerEventSources' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf namePrefix

instance Core.ToHeaders ListPartnerEventSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.ListPartnerEventSources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPartnerEventSources where
  toJSON ListPartnerEventSources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("NamePrefix" Core..= namePrefix)
          ]
      )

instance Core.ToPath ListPartnerEventSources where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPartnerEventSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPartnerEventSourcesResponse' smart constructor.
data ListPartnerEventSourcesResponse = ListPartnerEventSourcesResponse'
  { -- | A token you can use in a subsequent operation to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of partner event sources returned by the operation.
    partnerEventSources :: Prelude.Maybe [PartnerEventSource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPartnerEventSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPartnerEventSourcesResponse_nextToken' - A token you can use in a subsequent operation to retrieve the next set
-- of results.
--
-- 'partnerEventSources', 'listPartnerEventSourcesResponse_partnerEventSources' - The list of partner event sources returned by the operation.
--
-- 'httpStatus', 'listPartnerEventSourcesResponse_httpStatus' - The response's http status code.
newListPartnerEventSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPartnerEventSourcesResponse
newListPartnerEventSourcesResponse pHttpStatus_ =
  ListPartnerEventSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      partnerEventSources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use in a subsequent operation to retrieve the next set
-- of results.
listPartnerEventSourcesResponse_nextToken :: Lens.Lens' ListPartnerEventSourcesResponse (Prelude.Maybe Prelude.Text)
listPartnerEventSourcesResponse_nextToken = Lens.lens (\ListPartnerEventSourcesResponse' {nextToken} -> nextToken) (\s@ListPartnerEventSourcesResponse' {} a -> s {nextToken = a} :: ListPartnerEventSourcesResponse)

-- | The list of partner event sources returned by the operation.
listPartnerEventSourcesResponse_partnerEventSources :: Lens.Lens' ListPartnerEventSourcesResponse (Prelude.Maybe [PartnerEventSource])
listPartnerEventSourcesResponse_partnerEventSources = Lens.lens (\ListPartnerEventSourcesResponse' {partnerEventSources} -> partnerEventSources) (\s@ListPartnerEventSourcesResponse' {} a -> s {partnerEventSources = a} :: ListPartnerEventSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPartnerEventSourcesResponse_httpStatus :: Lens.Lens' ListPartnerEventSourcesResponse Prelude.Int
listPartnerEventSourcesResponse_httpStatus = Lens.lens (\ListPartnerEventSourcesResponse' {httpStatus} -> httpStatus) (\s@ListPartnerEventSourcesResponse' {} a -> s {httpStatus = a} :: ListPartnerEventSourcesResponse)

instance
  Prelude.NFData
    ListPartnerEventSourcesResponse
  where
  rnf ListPartnerEventSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf partnerEventSources
      `Prelude.seq` Prelude.rnf httpStatus
