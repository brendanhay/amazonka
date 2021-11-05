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
-- Module      : Network.AWS.EMRContainers.ListVirtualClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the specified virtual cluster. Virtual cluster
-- is a managed entity on Amazon EMR on EKS. You can create, describe, list
-- and delete virtual clusters. They do not consume any additional resource
-- in your system. A single virtual cluster maps to a single Kubernetes
-- namespace. Given this relationship, you can model virtual clusters the
-- same way you model Kubernetes namespaces to meet your requirements.
--
-- This operation returns paginated results.
module Network.AWS.EMRContainers.ListVirtualClusters
  ( -- * Creating a Request
    ListVirtualClusters (..),
    newListVirtualClusters,

    -- * Request Lenses
    listVirtualClusters_states,
    listVirtualClusters_createdAfter,
    listVirtualClusters_containerProviderType,
    listVirtualClusters_nextToken,
    listVirtualClusters_containerProviderId,
    listVirtualClusters_maxResults,
    listVirtualClusters_createdBefore,

    -- * Destructuring the Response
    ListVirtualClustersResponse (..),
    newListVirtualClustersResponse,

    -- * Response Lenses
    listVirtualClustersResponse_nextToken,
    listVirtualClustersResponse_virtualClusters,
    listVirtualClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMRContainers.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListVirtualClusters' smart constructor.
data ListVirtualClusters = ListVirtualClusters'
  { -- | The states of the requested virtual clusters.
    states :: Prelude.Maybe [VirtualClusterState],
    -- | The date and time after which the virtual clusters are created.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | The container provider type of the virtual cluster. EKS is the only
    -- supported type as of now.
    containerProviderType :: Prelude.Maybe ContainerProviderType,
    -- | The token for the next set of virtual clusters to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The container provider ID of the virtual cluster.
    containerProviderId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of virtual clusters that can be listed.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The date and time before which the virtual clusters are created.
    createdBefore :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'states', 'listVirtualClusters_states' - The states of the requested virtual clusters.
--
-- 'createdAfter', 'listVirtualClusters_createdAfter' - The date and time after which the virtual clusters are created.
--
-- 'containerProviderType', 'listVirtualClusters_containerProviderType' - The container provider type of the virtual cluster. EKS is the only
-- supported type as of now.
--
-- 'nextToken', 'listVirtualClusters_nextToken' - The token for the next set of virtual clusters to return.
--
-- 'containerProviderId', 'listVirtualClusters_containerProviderId' - The container provider ID of the virtual cluster.
--
-- 'maxResults', 'listVirtualClusters_maxResults' - The maximum number of virtual clusters that can be listed.
--
-- 'createdBefore', 'listVirtualClusters_createdBefore' - The date and time before which the virtual clusters are created.
newListVirtualClusters ::
  ListVirtualClusters
newListVirtualClusters =
  ListVirtualClusters'
    { states = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      containerProviderType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      containerProviderId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdBefore = Prelude.Nothing
    }

-- | The states of the requested virtual clusters.
listVirtualClusters_states :: Lens.Lens' ListVirtualClusters (Prelude.Maybe [VirtualClusterState])
listVirtualClusters_states = Lens.lens (\ListVirtualClusters' {states} -> states) (\s@ListVirtualClusters' {} a -> s {states = a} :: ListVirtualClusters) Prelude.. Lens.mapping Lens.coerced

-- | The date and time after which the virtual clusters are created.
listVirtualClusters_createdAfter :: Lens.Lens' ListVirtualClusters (Prelude.Maybe Prelude.UTCTime)
listVirtualClusters_createdAfter = Lens.lens (\ListVirtualClusters' {createdAfter} -> createdAfter) (\s@ListVirtualClusters' {} a -> s {createdAfter = a} :: ListVirtualClusters) Prelude.. Lens.mapping Core._Time

-- | The container provider type of the virtual cluster. EKS is the only
-- supported type as of now.
listVirtualClusters_containerProviderType :: Lens.Lens' ListVirtualClusters (Prelude.Maybe ContainerProviderType)
listVirtualClusters_containerProviderType = Lens.lens (\ListVirtualClusters' {containerProviderType} -> containerProviderType) (\s@ListVirtualClusters' {} a -> s {containerProviderType = a} :: ListVirtualClusters)

-- | The token for the next set of virtual clusters to return.
listVirtualClusters_nextToken :: Lens.Lens' ListVirtualClusters (Prelude.Maybe Prelude.Text)
listVirtualClusters_nextToken = Lens.lens (\ListVirtualClusters' {nextToken} -> nextToken) (\s@ListVirtualClusters' {} a -> s {nextToken = a} :: ListVirtualClusters)

-- | The container provider ID of the virtual cluster.
listVirtualClusters_containerProviderId :: Lens.Lens' ListVirtualClusters (Prelude.Maybe Prelude.Text)
listVirtualClusters_containerProviderId = Lens.lens (\ListVirtualClusters' {containerProviderId} -> containerProviderId) (\s@ListVirtualClusters' {} a -> s {containerProviderId = a} :: ListVirtualClusters)

-- | The maximum number of virtual clusters that can be listed.
listVirtualClusters_maxResults :: Lens.Lens' ListVirtualClusters (Prelude.Maybe Prelude.Int)
listVirtualClusters_maxResults = Lens.lens (\ListVirtualClusters' {maxResults} -> maxResults) (\s@ListVirtualClusters' {} a -> s {maxResults = a} :: ListVirtualClusters)

-- | The date and time before which the virtual clusters are created.
listVirtualClusters_createdBefore :: Lens.Lens' ListVirtualClusters (Prelude.Maybe Prelude.UTCTime)
listVirtualClusters_createdBefore = Lens.lens (\ListVirtualClusters' {createdBefore} -> createdBefore) (\s@ListVirtualClusters' {} a -> s {createdBefore = a} :: ListVirtualClusters) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListVirtualClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVirtualClustersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVirtualClustersResponse_virtualClusters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVirtualClusters_nextToken
          Lens..~ rs
          Lens.^? listVirtualClustersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListVirtualClusters where
  type
    AWSResponse ListVirtualClusters =
      ListVirtualClustersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualClustersResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "virtualClusters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVirtualClusters

instance Prelude.NFData ListVirtualClusters

instance Core.ToHeaders ListVirtualClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListVirtualClusters where
  toPath = Prelude.const "/virtualclusters"

instance Core.ToQuery ListVirtualClusters where
  toQuery ListVirtualClusters' {..} =
    Prelude.mconcat
      [ "states"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> states),
        "createdAfter" Core.=: createdAfter,
        "containerProviderType"
          Core.=: containerProviderType,
        "nextToken" Core.=: nextToken,
        "containerProviderId" Core.=: containerProviderId,
        "maxResults" Core.=: maxResults,
        "createdBefore" Core.=: createdBefore
      ]

-- | /See:/ 'newListVirtualClustersResponse' smart constructor.
data ListVirtualClustersResponse = ListVirtualClustersResponse'
  { -- | This output displays the token for the next set of virtual clusters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This output lists the specified virtual clusters.
    virtualClusters :: Prelude.Maybe [VirtualCluster],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualClustersResponse_nextToken' - This output displays the token for the next set of virtual clusters.
--
-- 'virtualClusters', 'listVirtualClustersResponse_virtualClusters' - This output lists the specified virtual clusters.
--
-- 'httpStatus', 'listVirtualClustersResponse_httpStatus' - The response's http status code.
newListVirtualClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVirtualClustersResponse
newListVirtualClustersResponse pHttpStatus_ =
  ListVirtualClustersResponse'
    { nextToken =
        Prelude.Nothing,
      virtualClusters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output displays the token for the next set of virtual clusters.
listVirtualClustersResponse_nextToken :: Lens.Lens' ListVirtualClustersResponse (Prelude.Maybe Prelude.Text)
listVirtualClustersResponse_nextToken = Lens.lens (\ListVirtualClustersResponse' {nextToken} -> nextToken) (\s@ListVirtualClustersResponse' {} a -> s {nextToken = a} :: ListVirtualClustersResponse)

-- | This output lists the specified virtual clusters.
listVirtualClustersResponse_virtualClusters :: Lens.Lens' ListVirtualClustersResponse (Prelude.Maybe [VirtualCluster])
listVirtualClustersResponse_virtualClusters = Lens.lens (\ListVirtualClustersResponse' {virtualClusters} -> virtualClusters) (\s@ListVirtualClustersResponse' {} a -> s {virtualClusters = a} :: ListVirtualClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVirtualClustersResponse_httpStatus :: Lens.Lens' ListVirtualClustersResponse Prelude.Int
listVirtualClustersResponse_httpStatus = Lens.lens (\ListVirtualClustersResponse' {httpStatus} -> httpStatus) (\s@ListVirtualClustersResponse' {} a -> s {httpStatus = a} :: ListVirtualClustersResponse)

instance Prelude.NFData ListVirtualClustersResponse
