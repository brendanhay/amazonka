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
-- Module      : Amazonka.Proton.ListEnvironmentAccountConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View a list of environment account connections.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListEnvironmentAccountConnections
  ( -- * Creating a Request
    ListEnvironmentAccountConnections (..),
    newListEnvironmentAccountConnections,

    -- * Request Lenses
    listEnvironmentAccountConnections_nextToken,
    listEnvironmentAccountConnections_environmentName,
    listEnvironmentAccountConnections_statuses,
    listEnvironmentAccountConnections_maxResults,
    listEnvironmentAccountConnections_requestedBy,

    -- * Destructuring the Response
    ListEnvironmentAccountConnectionsResponse (..),
    newListEnvironmentAccountConnectionsResponse,

    -- * Response Lenses
    listEnvironmentAccountConnectionsResponse_nextToken,
    listEnvironmentAccountConnectionsResponse_httpStatus,
    listEnvironmentAccountConnectionsResponse_environmentAccountConnections,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironmentAccountConnections' smart constructor.
data ListEnvironmentAccountConnections = ListEnvironmentAccountConnections'
  { -- | A token that indicates the location of the next environment account
    -- connection in the array of environment account connections, after the
    -- list of environment account connections that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The environment name that\'s associated with each listed environment
    -- account connection.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The status details for each listed environment account connection.
    statuses :: Prelude.Maybe [EnvironmentAccountConnectionStatus],
    -- | The maximum number of environment account connections to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of account making the @ListEnvironmentAccountConnections@
    -- request.
    requestedBy :: EnvironmentAccountConnectionRequesterAccountType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentAccountConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentAccountConnections_nextToken' - A token that indicates the location of the next environment account
-- connection in the array of environment account connections, after the
-- list of environment account connections that was previously requested.
--
-- 'environmentName', 'listEnvironmentAccountConnections_environmentName' - The environment name that\'s associated with each listed environment
-- account connection.
--
-- 'statuses', 'listEnvironmentAccountConnections_statuses' - The status details for each listed environment account connection.
--
-- 'maxResults', 'listEnvironmentAccountConnections_maxResults' - The maximum number of environment account connections to list.
--
-- 'requestedBy', 'listEnvironmentAccountConnections_requestedBy' - The type of account making the @ListEnvironmentAccountConnections@
-- request.
newListEnvironmentAccountConnections ::
  -- | 'requestedBy'
  EnvironmentAccountConnectionRequesterAccountType ->
  ListEnvironmentAccountConnections
newListEnvironmentAccountConnections pRequestedBy_ =
  ListEnvironmentAccountConnections'
    { nextToken =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      statuses = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      requestedBy = pRequestedBy_
    }

-- | A token that indicates the location of the next environment account
-- connection in the array of environment account connections, after the
-- list of environment account connections that was previously requested.
listEnvironmentAccountConnections_nextToken :: Lens.Lens' ListEnvironmentAccountConnections (Prelude.Maybe Prelude.Text)
listEnvironmentAccountConnections_nextToken = Lens.lens (\ListEnvironmentAccountConnections' {nextToken} -> nextToken) (\s@ListEnvironmentAccountConnections' {} a -> s {nextToken = a} :: ListEnvironmentAccountConnections)

-- | The environment name that\'s associated with each listed environment
-- account connection.
listEnvironmentAccountConnections_environmentName :: Lens.Lens' ListEnvironmentAccountConnections (Prelude.Maybe Prelude.Text)
listEnvironmentAccountConnections_environmentName = Lens.lens (\ListEnvironmentAccountConnections' {environmentName} -> environmentName) (\s@ListEnvironmentAccountConnections' {} a -> s {environmentName = a} :: ListEnvironmentAccountConnections)

-- | The status details for each listed environment account connection.
listEnvironmentAccountConnections_statuses :: Lens.Lens' ListEnvironmentAccountConnections (Prelude.Maybe [EnvironmentAccountConnectionStatus])
listEnvironmentAccountConnections_statuses = Lens.lens (\ListEnvironmentAccountConnections' {statuses} -> statuses) (\s@ListEnvironmentAccountConnections' {} a -> s {statuses = a} :: ListEnvironmentAccountConnections) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of environment account connections to list.
listEnvironmentAccountConnections_maxResults :: Lens.Lens' ListEnvironmentAccountConnections (Prelude.Maybe Prelude.Natural)
listEnvironmentAccountConnections_maxResults = Lens.lens (\ListEnvironmentAccountConnections' {maxResults} -> maxResults) (\s@ListEnvironmentAccountConnections' {} a -> s {maxResults = a} :: ListEnvironmentAccountConnections)

-- | The type of account making the @ListEnvironmentAccountConnections@
-- request.
listEnvironmentAccountConnections_requestedBy :: Lens.Lens' ListEnvironmentAccountConnections EnvironmentAccountConnectionRequesterAccountType
listEnvironmentAccountConnections_requestedBy = Lens.lens (\ListEnvironmentAccountConnections' {requestedBy} -> requestedBy) (\s@ListEnvironmentAccountConnections' {} a -> s {requestedBy = a} :: ListEnvironmentAccountConnections)

instance
  Core.AWSPager
    ListEnvironmentAccountConnections
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentAccountConnectionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEnvironmentAccountConnectionsResponse_environmentAccountConnections
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEnvironmentAccountConnections_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentAccountConnectionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListEnvironmentAccountConnections
  where
  type
    AWSResponse ListEnvironmentAccountConnections =
      ListEnvironmentAccountConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentAccountConnectionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Data..?> "environmentAccountConnections"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    ListEnvironmentAccountConnections
  where
  hashWithSalt
    _salt
    ListEnvironmentAccountConnections' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` statuses
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` requestedBy

instance
  Prelude.NFData
    ListEnvironmentAccountConnections
  where
  rnf ListEnvironmentAccountConnections' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf statuses
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf requestedBy

instance
  Data.ToHeaders
    ListEnvironmentAccountConnections
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListEnvironmentAccountConnections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListEnvironmentAccountConnections
  where
  toJSON ListEnvironmentAccountConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("environmentName" Data..=)
              Prelude.<$> environmentName,
            ("statuses" Data..=) Prelude.<$> statuses,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("requestedBy" Data..= requestedBy)
          ]
      )

instance
  Data.ToPath
    ListEnvironmentAccountConnections
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListEnvironmentAccountConnections
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnvironmentAccountConnectionsResponse' smart constructor.
data ListEnvironmentAccountConnectionsResponse = ListEnvironmentAccountConnectionsResponse'
  { -- | A token that indicates the location of the next environment account
    -- connection in the array of environment account connections, after the
    -- current requested list of environment account connections.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of environment account connections with details that\'s
    -- returned by Proton.
    environmentAccountConnections :: [EnvironmentAccountConnectionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentAccountConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentAccountConnectionsResponse_nextToken' - A token that indicates the location of the next environment account
-- connection in the array of environment account connections, after the
-- current requested list of environment account connections.
--
-- 'httpStatus', 'listEnvironmentAccountConnectionsResponse_httpStatus' - The response's http status code.
--
-- 'environmentAccountConnections', 'listEnvironmentAccountConnectionsResponse_environmentAccountConnections' - An array of environment account connections with details that\'s
-- returned by Proton.
newListEnvironmentAccountConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentAccountConnectionsResponse
newListEnvironmentAccountConnectionsResponse
  pHttpStatus_ =
    ListEnvironmentAccountConnectionsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        environmentAccountConnections =
          Prelude.mempty
      }

-- | A token that indicates the location of the next environment account
-- connection in the array of environment account connections, after the
-- current requested list of environment account connections.
listEnvironmentAccountConnectionsResponse_nextToken :: Lens.Lens' ListEnvironmentAccountConnectionsResponse (Prelude.Maybe Prelude.Text)
listEnvironmentAccountConnectionsResponse_nextToken = Lens.lens (\ListEnvironmentAccountConnectionsResponse' {nextToken} -> nextToken) (\s@ListEnvironmentAccountConnectionsResponse' {} a -> s {nextToken = a} :: ListEnvironmentAccountConnectionsResponse)

-- | The response's http status code.
listEnvironmentAccountConnectionsResponse_httpStatus :: Lens.Lens' ListEnvironmentAccountConnectionsResponse Prelude.Int
listEnvironmentAccountConnectionsResponse_httpStatus = Lens.lens (\ListEnvironmentAccountConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentAccountConnectionsResponse' {} a -> s {httpStatus = a} :: ListEnvironmentAccountConnectionsResponse)

-- | An array of environment account connections with details that\'s
-- returned by Proton.
listEnvironmentAccountConnectionsResponse_environmentAccountConnections :: Lens.Lens' ListEnvironmentAccountConnectionsResponse [EnvironmentAccountConnectionSummary]
listEnvironmentAccountConnectionsResponse_environmentAccountConnections = Lens.lens (\ListEnvironmentAccountConnectionsResponse' {environmentAccountConnections} -> environmentAccountConnections) (\s@ListEnvironmentAccountConnectionsResponse' {} a -> s {environmentAccountConnections = a} :: ListEnvironmentAccountConnectionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEnvironmentAccountConnectionsResponse
  where
  rnf ListEnvironmentAccountConnectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentAccountConnections
