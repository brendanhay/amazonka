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
-- Module      : Amazonka.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the number of node groups in a Global datastore
module Amazonka.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a Request
    DecreaseNodeGroupsInGlobalReplicationGroup (..),
    newDecreaseNodeGroupsInGlobalReplicationGroup,

    -- * Request Lenses
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove,
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain,
    decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId,
    decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount,
    decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately,

    -- * Destructuring the Response
    DecreaseNodeGroupsInGlobalReplicationGroupResponse (..),
    newDecreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- * Response Lenses
    decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup,
    decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDecreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroup = DecreaseNodeGroupsInGlobalReplicationGroup'
  { -- | If the value of NodeGroupCount is less than the current number of node
    -- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
    -- required. GlobalNodeGroupsToRemove is a list of NodeGroupIds to remove
    -- from the cluster. ElastiCache for Redis will attempt to remove all node
    -- groups listed by GlobalNodeGroupsToRemove from the cluster.
    globalNodeGroupsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | If the value of NodeGroupCount is less than the current number of node
    -- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
    -- required. GlobalNodeGroupsToRetain is a list of NodeGroupIds to retain
    -- from the cluster. ElastiCache for Redis will attempt to retain all node
    -- groups listed by GlobalNodeGroupsToRetain from the cluster.
    globalNodeGroupsToRetain :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | The number of node groups (shards) that results from the modification of
    -- the shard configuration
    nodeGroupCount :: Prelude.Int,
    -- | Indicates that the shard reconfiguration process begins immediately. At
    -- present, the only permitted value for this parameter is true.
    applyImmediately :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecreaseNodeGroupsInGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNodeGroupsToRemove', 'decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove' - If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. GlobalNodeGroupsToRemove is a list of NodeGroupIds to remove
-- from the cluster. ElastiCache for Redis will attempt to remove all node
-- groups listed by GlobalNodeGroupsToRemove from the cluster.
--
-- 'globalNodeGroupsToRetain', 'decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain' - If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. GlobalNodeGroupsToRetain is a list of NodeGroupIds to retain
-- from the cluster. ElastiCache for Redis will attempt to retain all node
-- groups listed by GlobalNodeGroupsToRetain from the cluster.
--
-- 'globalReplicationGroupId', 'decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
--
-- 'nodeGroupCount', 'decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount' - The number of node groups (shards) that results from the modification of
-- the shard configuration
--
-- 'applyImmediately', 'decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately' - Indicates that the shard reconfiguration process begins immediately. At
-- present, the only permitted value for this parameter is true.
newDecreaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Prelude.Text ->
  -- | 'nodeGroupCount'
  Prelude.Int ->
  -- | 'applyImmediately'
  Prelude.Bool ->
  DecreaseNodeGroupsInGlobalReplicationGroup
newDecreaseNodeGroupsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pNodeGroupCount_
  pApplyImmediately_ =
    DecreaseNodeGroupsInGlobalReplicationGroup'
      { globalNodeGroupsToRemove =
          Prelude.Nothing,
        globalNodeGroupsToRetain =
          Prelude.Nothing,
        globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        nodeGroupCount =
          pNodeGroupCount_,
        applyImmediately =
          pApplyImmediately_
      }

-- | If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. GlobalNodeGroupsToRemove is a list of NodeGroupIds to remove
-- from the cluster. ElastiCache for Redis will attempt to remove all node
-- groups listed by GlobalNodeGroupsToRemove from the cluster.
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Prelude.Maybe [Prelude.Text])
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {globalNodeGroupsToRemove} -> globalNodeGroupsToRemove) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {globalNodeGroupsToRemove = a} :: DecreaseNodeGroupsInGlobalReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. GlobalNodeGroupsToRetain is a list of NodeGroupIds to retain
-- from the cluster. ElastiCache for Redis will attempt to retain all node
-- groups listed by GlobalNodeGroupsToRetain from the cluster.
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Prelude.Maybe [Prelude.Text])
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {globalNodeGroupsToRetain} -> globalNodeGroupsToRetain) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {globalNodeGroupsToRetain = a} :: DecreaseNodeGroupsInGlobalReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Global datastore
decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Prelude.Text
decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)

-- | The number of node groups (shards) that results from the modification of
-- the shard configuration
decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Prelude.Int
decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {nodeGroupCount} -> nodeGroupCount) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {nodeGroupCount = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)

-- | Indicates that the shard reconfiguration process begins immediately. At
-- present, the only permitted value for this parameter is true.
decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Prelude.Bool
decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {applyImmediately} -> applyImmediately) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {applyImmediately = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)

instance
  Core.AWSRequest
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  type
    AWSResponse
      DecreaseNodeGroupsInGlobalReplicationGroup =
      DecreaseNodeGroupsInGlobalReplicationGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DecreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          DecreaseNodeGroupsInGlobalReplicationGroupResponse'
            Prelude.<$> (x Data..@? "GlobalReplicationGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  hashWithSalt
    _salt
    DecreaseNodeGroupsInGlobalReplicationGroup' {..} =
      _salt
        `Prelude.hashWithSalt` globalNodeGroupsToRemove
        `Prelude.hashWithSalt` globalNodeGroupsToRetain
        `Prelude.hashWithSalt` globalReplicationGroupId
        `Prelude.hashWithSalt` nodeGroupCount
        `Prelude.hashWithSalt` applyImmediately

instance
  Prelude.NFData
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  rnf DecreaseNodeGroupsInGlobalReplicationGroup' {..} =
    Prelude.rnf globalNodeGroupsToRemove
      `Prelude.seq` Prelude.rnf globalNodeGroupsToRetain
      `Prelude.seq` Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf nodeGroupCount
      `Prelude.seq` Prelude.rnf applyImmediately

instance
  Data.ToHeaders
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  toQuery
    DecreaseNodeGroupsInGlobalReplicationGroup' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DecreaseNodeGroupsInGlobalReplicationGroup" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2015-02-02" :: Prelude.ByteString),
          "GlobalNodeGroupsToRemove"
            Data.=: Data.toQuery
              ( Data.toQueryList "GlobalNodeGroupId"
                  Prelude.<$> globalNodeGroupsToRemove
              ),
          "GlobalNodeGroupsToRetain"
            Data.=: Data.toQuery
              ( Data.toQueryList "GlobalNodeGroupId"
                  Prelude.<$> globalNodeGroupsToRetain
              ),
          "GlobalReplicationGroupId"
            Data.=: globalReplicationGroupId,
          "NodeGroupCount" Data.=: nodeGroupCount,
          "ApplyImmediately" Data.=: applyImmediately
        ]

-- | /See:/ 'newDecreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroupResponse = DecreaseNodeGroupsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecreaseNodeGroupsInGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newDecreaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DecreaseNodeGroupsInGlobalReplicationGroupResponse
newDecreaseNodeGroupsInGlobalReplicationGroupResponse
  pHttpStatus_ =
    DecreaseNodeGroupsInGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@DecreaseNodeGroupsInGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: DecreaseNodeGroupsInGlobalReplicationGroupResponse)

-- | The response's http status code.
decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse Prelude.Int
decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@DecreaseNodeGroupsInGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: DecreaseNodeGroupsInGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    DecreaseNodeGroupsInGlobalReplicationGroupResponse
  where
  rnf
    DecreaseNodeGroupsInGlobalReplicationGroupResponse' {..} =
      Prelude.rnf globalReplicationGroup
        `Prelude.seq` Prelude.rnf httpStatus
