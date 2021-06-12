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
-- Module      : Network.AWS.CloudWatchEvents.ListApiDestinations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of API destination in the account in the current
-- Region.
module Network.AWS.CloudWatchEvents.ListApiDestinations
  ( -- * Creating a Request
    ListApiDestinations (..),
    newListApiDestinations,

    -- * Request Lenses
    listApiDestinations_nextToken,
    listApiDestinations_connectionArn,
    listApiDestinations_namePrefix,
    listApiDestinations_limit,

    -- * Destructuring the Response
    ListApiDestinationsResponse (..),
    newListApiDestinationsResponse,

    -- * Response Lenses
    listApiDestinationsResponse_nextToken,
    listApiDestinationsResponse_apiDestinations,
    listApiDestinationsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListApiDestinations' smart constructor.
data ListApiDestinations = ListApiDestinations'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The ARN of the connection specified for the API destination.
    connectionArn :: Core.Maybe Core.Text,
    -- | A name prefix to filter results returned. Only API destinations with a
    -- name that starts with the prefix are returned.
    namePrefix :: Core.Maybe Core.Text,
    -- | The maximum number of API destinations to include in the response.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApiDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApiDestinations_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'connectionArn', 'listApiDestinations_connectionArn' - The ARN of the connection specified for the API destination.
--
-- 'namePrefix', 'listApiDestinations_namePrefix' - A name prefix to filter results returned. Only API destinations with a
-- name that starts with the prefix are returned.
--
-- 'limit', 'listApiDestinations_limit' - The maximum number of API destinations to include in the response.
newListApiDestinations ::
  ListApiDestinations
newListApiDestinations =
  ListApiDestinations'
    { nextToken = Core.Nothing,
      connectionArn = Core.Nothing,
      namePrefix = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listApiDestinations_nextToken :: Lens.Lens' ListApiDestinations (Core.Maybe Core.Text)
listApiDestinations_nextToken = Lens.lens (\ListApiDestinations' {nextToken} -> nextToken) (\s@ListApiDestinations' {} a -> s {nextToken = a} :: ListApiDestinations)

-- | The ARN of the connection specified for the API destination.
listApiDestinations_connectionArn :: Lens.Lens' ListApiDestinations (Core.Maybe Core.Text)
listApiDestinations_connectionArn = Lens.lens (\ListApiDestinations' {connectionArn} -> connectionArn) (\s@ListApiDestinations' {} a -> s {connectionArn = a} :: ListApiDestinations)

-- | A name prefix to filter results returned. Only API destinations with a
-- name that starts with the prefix are returned.
listApiDestinations_namePrefix :: Lens.Lens' ListApiDestinations (Core.Maybe Core.Text)
listApiDestinations_namePrefix = Lens.lens (\ListApiDestinations' {namePrefix} -> namePrefix) (\s@ListApiDestinations' {} a -> s {namePrefix = a} :: ListApiDestinations)

-- | The maximum number of API destinations to include in the response.
listApiDestinations_limit :: Lens.Lens' ListApiDestinations (Core.Maybe Core.Natural)
listApiDestinations_limit = Lens.lens (\ListApiDestinations' {limit} -> limit) (\s@ListApiDestinations' {} a -> s {limit = a} :: ListApiDestinations)

instance Core.AWSRequest ListApiDestinations where
  type
    AWSResponse ListApiDestinations =
      ListApiDestinationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApiDestinationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ApiDestinations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApiDestinations

instance Core.NFData ListApiDestinations

instance Core.ToHeaders ListApiDestinations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.ListApiDestinations" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListApiDestinations where
  toJSON ListApiDestinations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ConnectionArn" Core..=) Core.<$> connectionArn,
            ("NamePrefix" Core..=) Core.<$> namePrefix,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListApiDestinations where
  toPath = Core.const "/"

instance Core.ToQuery ListApiDestinations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListApiDestinationsResponse' smart constructor.
data ListApiDestinationsResponse = ListApiDestinationsResponse'
  { -- | A token you can use in a subsequent request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @ApiDestination@ objects that include information about an
    -- API destination.
    apiDestinations :: Core.Maybe [ApiDestination],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApiDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApiDestinationsResponse_nextToken' - A token you can use in a subsequent request to retrieve the next set of
-- results.
--
-- 'apiDestinations', 'listApiDestinationsResponse_apiDestinations' - An array of @ApiDestination@ objects that include information about an
-- API destination.
--
-- 'httpStatus', 'listApiDestinationsResponse_httpStatus' - The response's http status code.
newListApiDestinationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListApiDestinationsResponse
newListApiDestinationsResponse pHttpStatus_ =
  ListApiDestinationsResponse'
    { nextToken =
        Core.Nothing,
      apiDestinations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use in a subsequent request to retrieve the next set of
-- results.
listApiDestinationsResponse_nextToken :: Lens.Lens' ListApiDestinationsResponse (Core.Maybe Core.Text)
listApiDestinationsResponse_nextToken = Lens.lens (\ListApiDestinationsResponse' {nextToken} -> nextToken) (\s@ListApiDestinationsResponse' {} a -> s {nextToken = a} :: ListApiDestinationsResponse)

-- | An array of @ApiDestination@ objects that include information about an
-- API destination.
listApiDestinationsResponse_apiDestinations :: Lens.Lens' ListApiDestinationsResponse (Core.Maybe [ApiDestination])
listApiDestinationsResponse_apiDestinations = Lens.lens (\ListApiDestinationsResponse' {apiDestinations} -> apiDestinations) (\s@ListApiDestinationsResponse' {} a -> s {apiDestinations = a} :: ListApiDestinationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listApiDestinationsResponse_httpStatus :: Lens.Lens' ListApiDestinationsResponse Core.Int
listApiDestinationsResponse_httpStatus = Lens.lens (\ListApiDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListApiDestinationsResponse' {} a -> s {httpStatus = a} :: ListApiDestinationsResponse)

instance Core.NFData ListApiDestinationsResponse
