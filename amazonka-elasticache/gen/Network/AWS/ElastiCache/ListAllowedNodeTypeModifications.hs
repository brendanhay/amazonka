{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available node types that you can scale your Redis cluster\'s
-- or replication group\'s current node type.
--
-- When you use the @ModifyCacheCluster@ or @ModifyReplicationGroup@
-- operations to scale your cluster or replication group, the value of the
-- @CacheNodeType@ parameter must be one of the node types returned by this
-- operation.
module Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
  ( -- * Creating a Request
    ListAllowedNodeTypeModifications (..),
    newListAllowedNodeTypeModifications,

    -- * Request Lenses
    listAllowedNodeTypeModifications_replicationGroupId,
    listAllowedNodeTypeModifications_cacheClusterId,

    -- * Destructuring the Response
    ListAllowedNodeTypeModificationsResponse (..),
    newListAllowedNodeTypeModificationsResponse,

    -- * Response Lenses
    listAllowedNodeTypeModificationsResponse_scaleUpModifications,
    listAllowedNodeTypeModificationsResponse_scaleDownModifications,
    listAllowedNodeTypeModificationsResponse_httpStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input parameters for the @ListAllowedNodeTypeModifications@
-- operation.
--
-- /See:/ 'newListAllowedNodeTypeModifications' smart constructor.
data ListAllowedNodeTypeModifications = ListAllowedNodeTypeModifications'
  { -- | The name of the replication group want to scale up to a larger node
    -- type. ElastiCache uses the replication group id to identify the current
    -- node type being used by this replication group, and from that to create
    -- a list of node types you can scale up to.
    --
    -- You must provide a value for either the @CacheClusterId@ or the
    -- @ReplicationGroupId@.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster you want to scale up to a larger node instanced
    -- type. ElastiCache uses the cluster id to identify the current node type
    -- of this cluster and from that to create a list of node types you can
    -- scale up to.
    --
    -- You must provide a value for either the @CacheClusterId@ or the
    -- @ReplicationGroupId@.
    cacheClusterId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAllowedNodeTypeModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'listAllowedNodeTypeModifications_replicationGroupId' - The name of the replication group want to scale up to a larger node
-- type. ElastiCache uses the replication group id to identify the current
-- node type being used by this replication group, and from that to create
-- a list of node types you can scale up to.
--
-- You must provide a value for either the @CacheClusterId@ or the
-- @ReplicationGroupId@.
--
-- 'cacheClusterId', 'listAllowedNodeTypeModifications_cacheClusterId' - The name of the cluster you want to scale up to a larger node instanced
-- type. ElastiCache uses the cluster id to identify the current node type
-- of this cluster and from that to create a list of node types you can
-- scale up to.
--
-- You must provide a value for either the @CacheClusterId@ or the
-- @ReplicationGroupId@.
newListAllowedNodeTypeModifications ::
  ListAllowedNodeTypeModifications
newListAllowedNodeTypeModifications =
  ListAllowedNodeTypeModifications'
    { replicationGroupId =
        Prelude.Nothing,
      cacheClusterId = Prelude.Nothing
    }

-- | The name of the replication group want to scale up to a larger node
-- type. ElastiCache uses the replication group id to identify the current
-- node type being used by this replication group, and from that to create
-- a list of node types you can scale up to.
--
-- You must provide a value for either the @CacheClusterId@ or the
-- @ReplicationGroupId@.
listAllowedNodeTypeModifications_replicationGroupId :: Lens.Lens' ListAllowedNodeTypeModifications (Prelude.Maybe Prelude.Text)
listAllowedNodeTypeModifications_replicationGroupId = Lens.lens (\ListAllowedNodeTypeModifications' {replicationGroupId} -> replicationGroupId) (\s@ListAllowedNodeTypeModifications' {} a -> s {replicationGroupId = a} :: ListAllowedNodeTypeModifications)

-- | The name of the cluster you want to scale up to a larger node instanced
-- type. ElastiCache uses the cluster id to identify the current node type
-- of this cluster and from that to create a list of node types you can
-- scale up to.
--
-- You must provide a value for either the @CacheClusterId@ or the
-- @ReplicationGroupId@.
listAllowedNodeTypeModifications_cacheClusterId :: Lens.Lens' ListAllowedNodeTypeModifications (Prelude.Maybe Prelude.Text)
listAllowedNodeTypeModifications_cacheClusterId = Lens.lens (\ListAllowedNodeTypeModifications' {cacheClusterId} -> cacheClusterId) (\s@ListAllowedNodeTypeModifications' {} a -> s {cacheClusterId = a} :: ListAllowedNodeTypeModifications)

instance
  Prelude.AWSRequest
    ListAllowedNodeTypeModifications
  where
  type
    Rs ListAllowedNodeTypeModifications =
      ListAllowedNodeTypeModificationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListAllowedNodeTypeModificationsResult"
      ( \s h x ->
          ListAllowedNodeTypeModificationsResponse'
            Prelude.<$> ( x Prelude..@? "ScaleUpModifications"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> ( x Prelude..@? "ScaleDownModifications"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAllowedNodeTypeModifications

instance
  Prelude.NFData
    ListAllowedNodeTypeModifications

instance
  Prelude.ToHeaders
    ListAllowedNodeTypeModifications
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ListAllowedNodeTypeModifications
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ListAllowedNodeTypeModifications
  where
  toQuery ListAllowedNodeTypeModifications' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ListAllowedNodeTypeModifications" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "ReplicationGroupId" Prelude.=: replicationGroupId,
        "CacheClusterId" Prelude.=: cacheClusterId
      ]

-- | Represents the allowed node types you can use to modify your cluster or
-- replication group.
--
-- /See:/ 'newListAllowedNodeTypeModificationsResponse' smart constructor.
data ListAllowedNodeTypeModificationsResponse = ListAllowedNodeTypeModificationsResponse'
  { -- | A string list, each element of which specifies a cache node type which
    -- you can use to scale your cluster or replication group.
    --
    -- When scaling up a Redis cluster or replication group using
    -- @ModifyCacheCluster@ or @ModifyReplicationGroup@, use a value from this
    -- list for the @CacheNodeType@ parameter.
    scaleUpModifications :: Prelude.Maybe [Prelude.Text],
    -- | A string list, each element of which specifies a cache node type which
    -- you can use to scale your cluster or replication group. When scaling
    -- down a Redis cluster or replication group using ModifyCacheCluster or
    -- ModifyReplicationGroup, use a value from this list for the CacheNodeType
    -- parameter.
    scaleDownModifications :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAllowedNodeTypeModificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scaleUpModifications', 'listAllowedNodeTypeModificationsResponse_scaleUpModifications' - A string list, each element of which specifies a cache node type which
-- you can use to scale your cluster or replication group.
--
-- When scaling up a Redis cluster or replication group using
-- @ModifyCacheCluster@ or @ModifyReplicationGroup@, use a value from this
-- list for the @CacheNodeType@ parameter.
--
-- 'scaleDownModifications', 'listAllowedNodeTypeModificationsResponse_scaleDownModifications' - A string list, each element of which specifies a cache node type which
-- you can use to scale your cluster or replication group. When scaling
-- down a Redis cluster or replication group using ModifyCacheCluster or
-- ModifyReplicationGroup, use a value from this list for the CacheNodeType
-- parameter.
--
-- 'httpStatus', 'listAllowedNodeTypeModificationsResponse_httpStatus' - The response's http status code.
newListAllowedNodeTypeModificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAllowedNodeTypeModificationsResponse
newListAllowedNodeTypeModificationsResponse
  pHttpStatus_ =
    ListAllowedNodeTypeModificationsResponse'
      { scaleUpModifications =
          Prelude.Nothing,
        scaleDownModifications =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A string list, each element of which specifies a cache node type which
-- you can use to scale your cluster or replication group.
--
-- When scaling up a Redis cluster or replication group using
-- @ModifyCacheCluster@ or @ModifyReplicationGroup@, use a value from this
-- list for the @CacheNodeType@ parameter.
listAllowedNodeTypeModificationsResponse_scaleUpModifications :: Lens.Lens' ListAllowedNodeTypeModificationsResponse (Prelude.Maybe [Prelude.Text])
listAllowedNodeTypeModificationsResponse_scaleUpModifications = Lens.lens (\ListAllowedNodeTypeModificationsResponse' {scaleUpModifications} -> scaleUpModifications) (\s@ListAllowedNodeTypeModificationsResponse' {} a -> s {scaleUpModifications = a} :: ListAllowedNodeTypeModificationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A string list, each element of which specifies a cache node type which
-- you can use to scale your cluster or replication group. When scaling
-- down a Redis cluster or replication group using ModifyCacheCluster or
-- ModifyReplicationGroup, use a value from this list for the CacheNodeType
-- parameter.
listAllowedNodeTypeModificationsResponse_scaleDownModifications :: Lens.Lens' ListAllowedNodeTypeModificationsResponse (Prelude.Maybe [Prelude.Text])
listAllowedNodeTypeModificationsResponse_scaleDownModifications = Lens.lens (\ListAllowedNodeTypeModificationsResponse' {scaleDownModifications} -> scaleDownModifications) (\s@ListAllowedNodeTypeModificationsResponse' {} a -> s {scaleDownModifications = a} :: ListAllowedNodeTypeModificationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listAllowedNodeTypeModificationsResponse_httpStatus :: Lens.Lens' ListAllowedNodeTypeModificationsResponse Prelude.Int
listAllowedNodeTypeModificationsResponse_httpStatus = Lens.lens (\ListAllowedNodeTypeModificationsResponse' {httpStatus} -> httpStatus) (\s@ListAllowedNodeTypeModificationsResponse' {} a -> s {httpStatus = a} :: ListAllowedNodeTypeModificationsResponse)

instance
  Prelude.NFData
    ListAllowedNodeTypeModificationsResponse
