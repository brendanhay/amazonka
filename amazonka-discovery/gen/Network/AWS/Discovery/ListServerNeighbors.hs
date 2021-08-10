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
-- Module      : Network.AWS.Discovery.ListServerNeighbors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of servers that are one network hop away from a
-- specified server.
module Network.AWS.Discovery.ListServerNeighbors
  ( -- * Creating a Request
    ListServerNeighbors (..),
    newListServerNeighbors,

    -- * Request Lenses
    listServerNeighbors_nextToken,
    listServerNeighbors_maxResults,
    listServerNeighbors_portInformationNeeded,
    listServerNeighbors_neighborConfigurationIds,
    listServerNeighbors_configurationId,

    -- * Destructuring the Response
    ListServerNeighborsResponse (..),
    newListServerNeighborsResponse,

    -- * Response Lenses
    listServerNeighborsResponse_nextToken,
    listServerNeighborsResponse_knownDependencyCount,
    listServerNeighborsResponse_httpStatus,
    listServerNeighborsResponse_neighbors,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListServerNeighbors' smart constructor.
data ListServerNeighbors = ListServerNeighbors'
  { -- | Token to retrieve the next set of results. For example, if you
    -- previously specified 100 IDs for
    -- @ListServerNeighborsRequest$neighborConfigurationIds@ but set
    -- @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10
    -- results along with a token. Use that token in this query to get the next
    -- set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return in a single page of output.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Flag to indicate if port and protocol information is needed as part of
    -- the response.
    portInformationNeeded :: Prelude.Maybe Prelude.Bool,
    -- | List of configuration IDs to test for one-hop-away.
    neighborConfigurationIds :: Prelude.Maybe [Prelude.Text],
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
-- 'nextToken', 'listServerNeighbors_nextToken' - Token to retrieve the next set of results. For example, if you
-- previously specified 100 IDs for
-- @ListServerNeighborsRequest$neighborConfigurationIds@ but set
-- @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
--
-- 'maxResults', 'listServerNeighbors_maxResults' - Maximum number of results to return in a single page of output.
--
-- 'portInformationNeeded', 'listServerNeighbors_portInformationNeeded' - Flag to indicate if port and protocol information is needed as part of
-- the response.
--
-- 'neighborConfigurationIds', 'listServerNeighbors_neighborConfigurationIds' - List of configuration IDs to test for one-hop-away.
--
-- 'configurationId', 'listServerNeighbors_configurationId' - Configuration ID of the server for which neighbors are being listed.
newListServerNeighbors ::
  -- | 'configurationId'
  Prelude.Text ->
  ListServerNeighbors
newListServerNeighbors pConfigurationId_ =
  ListServerNeighbors'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      portInformationNeeded = Prelude.Nothing,
      neighborConfigurationIds = Prelude.Nothing,
      configurationId = pConfigurationId_
    }

-- | Token to retrieve the next set of results. For example, if you
-- previously specified 100 IDs for
-- @ListServerNeighborsRequest$neighborConfigurationIds@ but set
-- @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
listServerNeighbors_nextToken :: Lens.Lens' ListServerNeighbors (Prelude.Maybe Prelude.Text)
listServerNeighbors_nextToken = Lens.lens (\ListServerNeighbors' {nextToken} -> nextToken) (\s@ListServerNeighbors' {} a -> s {nextToken = a} :: ListServerNeighbors)

-- | Maximum number of results to return in a single page of output.
listServerNeighbors_maxResults :: Lens.Lens' ListServerNeighbors (Prelude.Maybe Prelude.Int)
listServerNeighbors_maxResults = Lens.lens (\ListServerNeighbors' {maxResults} -> maxResults) (\s@ListServerNeighbors' {} a -> s {maxResults = a} :: ListServerNeighbors)

-- | Flag to indicate if port and protocol information is needed as part of
-- the response.
listServerNeighbors_portInformationNeeded :: Lens.Lens' ListServerNeighbors (Prelude.Maybe Prelude.Bool)
listServerNeighbors_portInformationNeeded = Lens.lens (\ListServerNeighbors' {portInformationNeeded} -> portInformationNeeded) (\s@ListServerNeighbors' {} a -> s {portInformationNeeded = a} :: ListServerNeighbors)

-- | List of configuration IDs to test for one-hop-away.
listServerNeighbors_neighborConfigurationIds :: Lens.Lens' ListServerNeighbors (Prelude.Maybe [Prelude.Text])
listServerNeighbors_neighborConfigurationIds = Lens.lens (\ListServerNeighbors' {neighborConfigurationIds} -> neighborConfigurationIds) (\s@ListServerNeighbors' {} a -> s {neighborConfigurationIds = a} :: ListServerNeighbors) Prelude.. Lens.mapping Lens._Coerce

-- | Configuration ID of the server for which neighbors are being listed.
listServerNeighbors_configurationId :: Lens.Lens' ListServerNeighbors Prelude.Text
listServerNeighbors_configurationId = Lens.lens (\ListServerNeighbors' {configurationId} -> configurationId) (\s@ListServerNeighbors' {} a -> s {configurationId = a} :: ListServerNeighbors)

instance Core.AWSRequest ListServerNeighbors where
  type
    AWSResponse ListServerNeighbors =
      ListServerNeighborsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServerNeighborsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "knownDependencyCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "neighbors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListServerNeighbors

instance Prelude.NFData ListServerNeighbors

instance Core.ToHeaders ListServerNeighbors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.ListServerNeighbors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListServerNeighbors where
  toJSON ListServerNeighbors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("portInformationNeeded" Core..=)
              Prelude.<$> portInformationNeeded,
            ("neighborConfigurationIds" Core..=)
              Prelude.<$> neighborConfigurationIds,
            Prelude.Just
              ("configurationId" Core..= configurationId)
          ]
      )

instance Core.ToPath ListServerNeighbors where
  toPath = Prelude.const "/"

instance Core.ToQuery ListServerNeighbors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServerNeighborsResponse' smart constructor.
data ListServerNeighborsResponse = ListServerNeighborsResponse'
  { -- | Token to retrieve the next set of results. For example, if you specified
    -- 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but
    -- set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of
    -- 10 results along with this token. Use this token in the next query to
    -- retrieve the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Count of distinct servers that are one hop away from the given server.
    knownDependencyCount :: Prelude.Maybe Prelude.Integer,
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
-- 'nextToken', 'listServerNeighborsResponse_nextToken' - Token to retrieve the next set of results. For example, if you specified
-- 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but
-- set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of
-- 10 results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
--
-- 'knownDependencyCount', 'listServerNeighborsResponse_knownDependencyCount' - Count of distinct servers that are one hop away from the given server.
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
    { nextToken =
        Prelude.Nothing,
      knownDependencyCount = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      neighbors = Prelude.mempty
    }

-- | Token to retrieve the next set of results. For example, if you specified
-- 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but
-- set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of
-- 10 results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
listServerNeighborsResponse_nextToken :: Lens.Lens' ListServerNeighborsResponse (Prelude.Maybe Prelude.Text)
listServerNeighborsResponse_nextToken = Lens.lens (\ListServerNeighborsResponse' {nextToken} -> nextToken) (\s@ListServerNeighborsResponse' {} a -> s {nextToken = a} :: ListServerNeighborsResponse)

-- | Count of distinct servers that are one hop away from the given server.
listServerNeighborsResponse_knownDependencyCount :: Lens.Lens' ListServerNeighborsResponse (Prelude.Maybe Prelude.Integer)
listServerNeighborsResponse_knownDependencyCount = Lens.lens (\ListServerNeighborsResponse' {knownDependencyCount} -> knownDependencyCount) (\s@ListServerNeighborsResponse' {} a -> s {knownDependencyCount = a} :: ListServerNeighborsResponse)

-- | The response's http status code.
listServerNeighborsResponse_httpStatus :: Lens.Lens' ListServerNeighborsResponse Prelude.Int
listServerNeighborsResponse_httpStatus = Lens.lens (\ListServerNeighborsResponse' {httpStatus} -> httpStatus) (\s@ListServerNeighborsResponse' {} a -> s {httpStatus = a} :: ListServerNeighborsResponse)

-- | List of distinct servers that are one hop away from the given server.
listServerNeighborsResponse_neighbors :: Lens.Lens' ListServerNeighborsResponse [NeighborConnectionDetail]
listServerNeighborsResponse_neighbors = Lens.lens (\ListServerNeighborsResponse' {neighbors} -> neighbors) (\s@ListServerNeighborsResponse' {} a -> s {neighbors = a} :: ListServerNeighborsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListServerNeighborsResponse
