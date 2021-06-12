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
-- Module      : Network.AWS.CloudWatchEvents.ListPartnerEventSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list all the partner event
-- source names that they have created. This operation is not used by AWS
-- customers.
module Network.AWS.CloudWatchEvents.ListPartnerEventSources
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

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPartnerEventSources' smart constructor.
data ListPartnerEventSources = ListPartnerEventSources'
  { -- | The token returned by a previous call to this operation. Specifying this
    -- retrieves the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | pecifying this limits the number of results returned by this operation.
    -- The operation also returns a NextToken which you can use in a subsequent
    -- operation to retrieve the next set of results.
    limit :: Core.Maybe Core.Natural,
    -- | If you specify this, the results are limited to only those partner event
    -- sources that start with the string you specify.
    namePrefix :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListPartnerEventSources
newListPartnerEventSources pNamePrefix_ =
  ListPartnerEventSources'
    { nextToken = Core.Nothing,
      limit = Core.Nothing,
      namePrefix = pNamePrefix_
    }

-- | The token returned by a previous call to this operation. Specifying this
-- retrieves the next set of results.
listPartnerEventSources_nextToken :: Lens.Lens' ListPartnerEventSources (Core.Maybe Core.Text)
listPartnerEventSources_nextToken = Lens.lens (\ListPartnerEventSources' {nextToken} -> nextToken) (\s@ListPartnerEventSources' {} a -> s {nextToken = a} :: ListPartnerEventSources)

-- | pecifying this limits the number of results returned by this operation.
-- The operation also returns a NextToken which you can use in a subsequent
-- operation to retrieve the next set of results.
listPartnerEventSources_limit :: Lens.Lens' ListPartnerEventSources (Core.Maybe Core.Natural)
listPartnerEventSources_limit = Lens.lens (\ListPartnerEventSources' {limit} -> limit) (\s@ListPartnerEventSources' {} a -> s {limit = a} :: ListPartnerEventSources)

-- | If you specify this, the results are limited to only those partner event
-- sources that start with the string you specify.
listPartnerEventSources_namePrefix :: Lens.Lens' ListPartnerEventSources Core.Text
listPartnerEventSources_namePrefix = Lens.lens (\ListPartnerEventSources' {namePrefix} -> namePrefix) (\s@ListPartnerEventSources' {} a -> s {namePrefix = a} :: ListPartnerEventSources)

instance Core.AWSRequest ListPartnerEventSources where
  type
    AWSResponse ListPartnerEventSources =
      ListPartnerEventSourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPartnerEventSourcesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PartnerEventSources"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPartnerEventSources

instance Core.NFData ListPartnerEventSources

instance Core.ToHeaders ListPartnerEventSources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.ListPartnerEventSources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPartnerEventSources where
  toJSON ListPartnerEventSources' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("NamePrefix" Core..= namePrefix)
          ]
      )

instance Core.ToPath ListPartnerEventSources where
  toPath = Core.const "/"

instance Core.ToQuery ListPartnerEventSources where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPartnerEventSourcesResponse' smart constructor.
data ListPartnerEventSourcesResponse = ListPartnerEventSourcesResponse'
  { -- | A token you can use in a subsequent operation to retrieve the next set
    -- of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of partner event sources returned by the operation.
    partnerEventSources :: Core.Maybe [PartnerEventSource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListPartnerEventSourcesResponse
newListPartnerEventSourcesResponse pHttpStatus_ =
  ListPartnerEventSourcesResponse'
    { nextToken =
        Core.Nothing,
      partnerEventSources = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use in a subsequent operation to retrieve the next set
-- of results.
listPartnerEventSourcesResponse_nextToken :: Lens.Lens' ListPartnerEventSourcesResponse (Core.Maybe Core.Text)
listPartnerEventSourcesResponse_nextToken = Lens.lens (\ListPartnerEventSourcesResponse' {nextToken} -> nextToken) (\s@ListPartnerEventSourcesResponse' {} a -> s {nextToken = a} :: ListPartnerEventSourcesResponse)

-- | The list of partner event sources returned by the operation.
listPartnerEventSourcesResponse_partnerEventSources :: Lens.Lens' ListPartnerEventSourcesResponse (Core.Maybe [PartnerEventSource])
listPartnerEventSourcesResponse_partnerEventSources = Lens.lens (\ListPartnerEventSourcesResponse' {partnerEventSources} -> partnerEventSources) (\s@ListPartnerEventSourcesResponse' {} a -> s {partnerEventSources = a} :: ListPartnerEventSourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPartnerEventSourcesResponse_httpStatus :: Lens.Lens' ListPartnerEventSourcesResponse Core.Int
listPartnerEventSourcesResponse_httpStatus = Lens.lens (\ListPartnerEventSourcesResponse' {httpStatus} -> httpStatus) (\s@ListPartnerEventSourcesResponse' {} a -> s {httpStatus = a} :: ListPartnerEventSourcesResponse)

instance Core.NFData ListPartnerEventSourcesResponse
