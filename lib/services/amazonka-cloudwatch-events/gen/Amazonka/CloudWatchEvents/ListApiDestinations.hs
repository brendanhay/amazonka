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
-- Module      : Amazonka.CloudWatchEvents.ListApiDestinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of API destination in the account in the current
-- Region.
module Amazonka.CloudWatchEvents.ListApiDestinations
  ( -- * Creating a Request
    ListApiDestinations (..),
    newListApiDestinations,

    -- * Request Lenses
    listApiDestinations_connectionArn,
    listApiDestinations_limit,
    listApiDestinations_namePrefix,
    listApiDestinations_nextToken,

    -- * Destructuring the Response
    ListApiDestinationsResponse (..),
    newListApiDestinationsResponse,

    -- * Response Lenses
    listApiDestinationsResponse_apiDestinations,
    listApiDestinationsResponse_nextToken,
    listApiDestinationsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApiDestinations' smart constructor.
data ListApiDestinations = ListApiDestinations'
  { -- | The ARN of the connection specified for the API destination.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of API destinations to include in the response.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A name prefix to filter results returned. Only API destinations with a
    -- name that starts with the prefix are returned.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApiDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionArn', 'listApiDestinations_connectionArn' - The ARN of the connection specified for the API destination.
--
-- 'limit', 'listApiDestinations_limit' - The maximum number of API destinations to include in the response.
--
-- 'namePrefix', 'listApiDestinations_namePrefix' - A name prefix to filter results returned. Only API destinations with a
-- name that starts with the prefix are returned.
--
-- 'nextToken', 'listApiDestinations_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
newListApiDestinations ::
  ListApiDestinations
newListApiDestinations =
  ListApiDestinations'
    { connectionArn =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ARN of the connection specified for the API destination.
listApiDestinations_connectionArn :: Lens.Lens' ListApiDestinations (Prelude.Maybe Prelude.Text)
listApiDestinations_connectionArn = Lens.lens (\ListApiDestinations' {connectionArn} -> connectionArn) (\s@ListApiDestinations' {} a -> s {connectionArn = a} :: ListApiDestinations)

-- | The maximum number of API destinations to include in the response.
listApiDestinations_limit :: Lens.Lens' ListApiDestinations (Prelude.Maybe Prelude.Natural)
listApiDestinations_limit = Lens.lens (\ListApiDestinations' {limit} -> limit) (\s@ListApiDestinations' {} a -> s {limit = a} :: ListApiDestinations)

-- | A name prefix to filter results returned. Only API destinations with a
-- name that starts with the prefix are returned.
listApiDestinations_namePrefix :: Lens.Lens' ListApiDestinations (Prelude.Maybe Prelude.Text)
listApiDestinations_namePrefix = Lens.lens (\ListApiDestinations' {namePrefix} -> namePrefix) (\s@ListApiDestinations' {} a -> s {namePrefix = a} :: ListApiDestinations)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listApiDestinations_nextToken :: Lens.Lens' ListApiDestinations (Prelude.Maybe Prelude.Text)
listApiDestinations_nextToken = Lens.lens (\ListApiDestinations' {nextToken} -> nextToken) (\s@ListApiDestinations' {} a -> s {nextToken = a} :: ListApiDestinations)

instance Core.AWSRequest ListApiDestinations where
  type
    AWSResponse ListApiDestinations =
      ListApiDestinationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApiDestinationsResponse'
            Prelude.<$> ( x
                            Data..?> "ApiDestinations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApiDestinations where
  hashWithSalt _salt ListApiDestinations' {..} =
    _salt
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApiDestinations where
  rnf ListApiDestinations' {..} =
    Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListApiDestinations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.ListApiDestinations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApiDestinations where
  toJSON ListApiDestinations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionArn" Data..=) Prelude.<$> connectionArn,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NamePrefix" Data..=) Prelude.<$> namePrefix,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListApiDestinations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApiDestinations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApiDestinationsResponse' smart constructor.
data ListApiDestinationsResponse = ListApiDestinationsResponse'
  { -- | An array of @ApiDestination@ objects that include information about an
    -- API destination.
    apiDestinations :: Prelude.Maybe [ApiDestination],
    -- | A token you can use in a subsequent request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApiDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiDestinations', 'listApiDestinationsResponse_apiDestinations' - An array of @ApiDestination@ objects that include information about an
-- API destination.
--
-- 'nextToken', 'listApiDestinationsResponse_nextToken' - A token you can use in a subsequent request to retrieve the next set of
-- results.
--
-- 'httpStatus', 'listApiDestinationsResponse_httpStatus' - The response's http status code.
newListApiDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApiDestinationsResponse
newListApiDestinationsResponse pHttpStatus_ =
  ListApiDestinationsResponse'
    { apiDestinations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @ApiDestination@ objects that include information about an
-- API destination.
listApiDestinationsResponse_apiDestinations :: Lens.Lens' ListApiDestinationsResponse (Prelude.Maybe [ApiDestination])
listApiDestinationsResponse_apiDestinations = Lens.lens (\ListApiDestinationsResponse' {apiDestinations} -> apiDestinations) (\s@ListApiDestinationsResponse' {} a -> s {apiDestinations = a} :: ListApiDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token you can use in a subsequent request to retrieve the next set of
-- results.
listApiDestinationsResponse_nextToken :: Lens.Lens' ListApiDestinationsResponse (Prelude.Maybe Prelude.Text)
listApiDestinationsResponse_nextToken = Lens.lens (\ListApiDestinationsResponse' {nextToken} -> nextToken) (\s@ListApiDestinationsResponse' {} a -> s {nextToken = a} :: ListApiDestinationsResponse)

-- | The response's http status code.
listApiDestinationsResponse_httpStatus :: Lens.Lens' ListApiDestinationsResponse Prelude.Int
listApiDestinationsResponse_httpStatus = Lens.lens (\ListApiDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListApiDestinationsResponse' {} a -> s {httpStatus = a} :: ListApiDestinationsResponse)

instance Prelude.NFData ListApiDestinationsResponse where
  rnf ListApiDestinationsResponse' {..} =
    Prelude.rnf apiDestinations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
