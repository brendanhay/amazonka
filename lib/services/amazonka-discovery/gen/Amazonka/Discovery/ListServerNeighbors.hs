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
-- Module      : Amazonka.Discovery.ListServerNeighbors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of servers that are one network hop away from a
-- specified server.
module Amazonka.Discovery.ListServerNeighbors
  ( -- * Creating a Request
    ListServerNeighbors (..),
    newListServerNeighbors,

    -- * Request Lenses
    listServerNeighbors_maxResults,
    listServerNeighbors_neighborConfigurationIds,
    listServerNeighbors_nextToken,
    listServerNeighbors_portInformationNeeded,
    listServerNeighbors_configurationId,

    -- * Destructuring the Response
    ListServerNeighborsResponse (..),
    newListServerNeighborsResponse,

    -- * Response Lenses
    listServerNeighborsResponse_knownDependencyCount,
    listServerNeighborsResponse_nextToken,
    listServerNeighborsResponse_httpStatus,
    listServerNeighborsResponse_neighbors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServerNeighbors' smart constructor.
data ListServerNeighbors = ListServerNeighbors'
  { -- | Maximum number of results to return in a single page of output.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | List of configuration IDs to test for one-hop-away.
    neighborConfigurationIds :: Prelude.Maybe [Prelude.Text],
    -- | Token to retrieve the next set of results. For example, if you
    -- previously specified 100 IDs for
    -- @ListServerNeighborsRequest$neighborConfigurationIds@ but set
    -- @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10
    -- results along with a token. Use that token in this query to get the next
    -- set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Flag to indicate if port and protocol information is needed as part of
    -- the response.
    portInformationNeeded :: Prelude.Maybe Prelude.Bool,
    -- | Configuration ID of the server for which neighbors are being listed.
    configurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServerNeighbors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listServerNeighbors_maxResults' - Maximum number of results to return in a single page of output.
--
-- 'neighborConfigurationIds', 'listServerNeighbors_neighborConfigurationIds' - List of configuration IDs to test for one-hop-away.
--
-- 'nextToken', 'listServerNeighbors_nextToken' - Token to retrieve the next set of results. For example, if you
-- previously specified 100 IDs for
-- @ListServerNeighborsRequest$neighborConfigurationIds@ but set
-- @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
--
-- 'portInformationNeeded', 'listServerNeighbors_portInformationNeeded' - Flag to indicate if port and protocol information is needed as part of
-- the response.
--
-- 'configurationId', 'listServerNeighbors_configurationId' - Configuration ID of the server for which neighbors are being listed.
newListServerNeighbors ::
  -- | 'configurationId'
  Prelude.Text ->
  ListServerNeighbors
newListServerNeighbors pConfigurationId_ =
  ListServerNeighbors'
    { maxResults = Prelude.Nothing,
      neighborConfigurationIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      portInformationNeeded = Prelude.Nothing,
      configurationId = pConfigurationId_
    }

-- | Maximum number of results to return in a single page of output.
listServerNeighbors_maxResults :: Lens.Lens' ListServerNeighbors (Prelude.Maybe Prelude.Int)
listServerNeighbors_maxResults = Lens.lens (\ListServerNeighbors' {maxResults} -> maxResults) (\s@ListServerNeighbors' {} a -> s {maxResults = a} :: ListServerNeighbors)

-- | List of configuration IDs to test for one-hop-away.
listServerNeighbors_neighborConfigurationIds :: Lens.Lens' ListServerNeighbors (Prelude.Maybe [Prelude.Text])
listServerNeighbors_neighborConfigurationIds = Lens.lens (\ListServerNeighbors' {neighborConfigurationIds} -> neighborConfigurationIds) (\s@ListServerNeighbors' {} a -> s {neighborConfigurationIds = a} :: ListServerNeighbors) Prelude.. Lens.mapping Lens.coerced

-- | Token to retrieve the next set of results. For example, if you
-- previously specified 100 IDs for
-- @ListServerNeighborsRequest$neighborConfigurationIds@ but set
-- @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
listServerNeighbors_nextToken :: Lens.Lens' ListServerNeighbors (Prelude.Maybe Prelude.Text)
listServerNeighbors_nextToken = Lens.lens (\ListServerNeighbors' {nextToken} -> nextToken) (\s@ListServerNeighbors' {} a -> s {nextToken = a} :: ListServerNeighbors)

-- | Flag to indicate if port and protocol information is needed as part of
-- the response.
listServerNeighbors_portInformationNeeded :: Lens.Lens' ListServerNeighbors (Prelude.Maybe Prelude.Bool)
listServerNeighbors_portInformationNeeded = Lens.lens (\ListServerNeighbors' {portInformationNeeded} -> portInformationNeeded) (\s@ListServerNeighbors' {} a -> s {portInformationNeeded = a} :: ListServerNeighbors)

-- | Configuration ID of the server for which neighbors are being listed.
listServerNeighbors_configurationId :: Lens.Lens' ListServerNeighbors Prelude.Text
listServerNeighbors_configurationId = Lens.lens (\ListServerNeighbors' {configurationId} -> configurationId) (\s@ListServerNeighbors' {} a -> s {configurationId = a} :: ListServerNeighbors)

instance Core.AWSRequest ListServerNeighbors where
  type
    AWSResponse ListServerNeighbors =
      ListServerNeighborsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServerNeighborsResponse'
            Prelude.<$> (x Data..?> "knownDependencyCount")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "neighbors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListServerNeighbors where
  hashWithSalt _salt ListServerNeighbors' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` neighborConfigurationIds
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` portInformationNeeded
      `Prelude.hashWithSalt` configurationId

instance Prelude.NFData ListServerNeighbors where
  rnf ListServerNeighbors' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf neighborConfigurationIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf portInformationNeeded
      `Prelude.seq` Prelude.rnf configurationId

instance Data.ToHeaders ListServerNeighbors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.ListServerNeighbors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServerNeighbors where
  toJSON ListServerNeighbors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("neighborConfigurationIds" Data..=)
              Prelude.<$> neighborConfigurationIds,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("portInformationNeeded" Data..=)
              Prelude.<$> portInformationNeeded,
            Prelude.Just
              ("configurationId" Data..= configurationId)
          ]
      )

instance Data.ToPath ListServerNeighbors where
  toPath = Prelude.const "/"

instance Data.ToQuery ListServerNeighbors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServerNeighborsResponse' smart constructor.
data ListServerNeighborsResponse = ListServerNeighborsResponse'
  { -- | Count of distinct servers that are one hop away from the given server.
    knownDependencyCount :: Prelude.Maybe Prelude.Integer,
    -- | Token to retrieve the next set of results. For example, if you specified
    -- 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but
    -- set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of
    -- 10 results along with this token. Use this token in the next query to
    -- retrieve the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of distinct servers that are one hop away from the given server.
    neighbors :: [NeighborConnectionDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServerNeighborsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knownDependencyCount', 'listServerNeighborsResponse_knownDependencyCount' - Count of distinct servers that are one hop away from the given server.
--
-- 'nextToken', 'listServerNeighborsResponse_nextToken' - Token to retrieve the next set of results. For example, if you specified
-- 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but
-- set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of
-- 10 results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
--
-- 'httpStatus', 'listServerNeighborsResponse_httpStatus' - The response's http status code.
--
-- 'neighbors', 'listServerNeighborsResponse_neighbors' - List of distinct servers that are one hop away from the given server.
newListServerNeighborsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServerNeighborsResponse
newListServerNeighborsResponse pHttpStatus_ =
  ListServerNeighborsResponse'
    { knownDependencyCount =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      neighbors = Prelude.mempty
    }

-- | Count of distinct servers that are one hop away from the given server.
listServerNeighborsResponse_knownDependencyCount :: Lens.Lens' ListServerNeighborsResponse (Prelude.Maybe Prelude.Integer)
listServerNeighborsResponse_knownDependencyCount = Lens.lens (\ListServerNeighborsResponse' {knownDependencyCount} -> knownDependencyCount) (\s@ListServerNeighborsResponse' {} a -> s {knownDependencyCount = a} :: ListServerNeighborsResponse)

-- | Token to retrieve the next set of results. For example, if you specified
-- 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but
-- set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of
-- 10 results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
listServerNeighborsResponse_nextToken :: Lens.Lens' ListServerNeighborsResponse (Prelude.Maybe Prelude.Text)
listServerNeighborsResponse_nextToken = Lens.lens (\ListServerNeighborsResponse' {nextToken} -> nextToken) (\s@ListServerNeighborsResponse' {} a -> s {nextToken = a} :: ListServerNeighborsResponse)

-- | The response's http status code.
listServerNeighborsResponse_httpStatus :: Lens.Lens' ListServerNeighborsResponse Prelude.Int
listServerNeighborsResponse_httpStatus = Lens.lens (\ListServerNeighborsResponse' {httpStatus} -> httpStatus) (\s@ListServerNeighborsResponse' {} a -> s {httpStatus = a} :: ListServerNeighborsResponse)

-- | List of distinct servers that are one hop away from the given server.
listServerNeighborsResponse_neighbors :: Lens.Lens' ListServerNeighborsResponse [NeighborConnectionDetail]
listServerNeighborsResponse_neighbors = Lens.lens (\ListServerNeighborsResponse' {neighbors} -> neighbors) (\s@ListServerNeighborsResponse' {} a -> s {neighbors = a} :: ListServerNeighborsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListServerNeighborsResponse where
  rnf ListServerNeighborsResponse' {..} =
    Prelude.rnf knownDependencyCount
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf neighbors
