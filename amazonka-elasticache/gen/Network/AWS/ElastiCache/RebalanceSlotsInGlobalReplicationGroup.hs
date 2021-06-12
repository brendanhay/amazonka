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
-- Module      : Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redistribute slots to ensure uniform distribution across existing shards
-- in the cluster.
module Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
  ( -- * Creating a Request
    RebalanceSlotsInGlobalReplicationGroup (..),
    newRebalanceSlotsInGlobalReplicationGroup,

    -- * Request Lenses
    rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId,
    rebalanceSlotsInGlobalReplicationGroup_applyImmediately,

    -- * Destructuring the Response
    RebalanceSlotsInGlobalReplicationGroupResponse (..),
    newRebalanceSlotsInGlobalReplicationGroupResponse,

    -- * Response Lenses
    rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup,
    rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebalanceSlotsInGlobalReplicationGroup' smart constructor.
data RebalanceSlotsInGlobalReplicationGroup = RebalanceSlotsInGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Text,
    -- | If @True@, redistribution is applied immediately.
    applyImmediately :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebalanceSlotsInGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupId', 'rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global Datastore
--
-- 'applyImmediately', 'rebalanceSlotsInGlobalReplicationGroup_applyImmediately' - If @True@, redistribution is applied immediately.
newRebalanceSlotsInGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Core.Text ->
  -- | 'applyImmediately'
  Core.Bool ->
  RebalanceSlotsInGlobalReplicationGroup
newRebalanceSlotsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pApplyImmediately_ =
    RebalanceSlotsInGlobalReplicationGroup'
      { globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        applyImmediately =
          pApplyImmediately_
      }

-- | The name of the Global Datastore
rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Core.Text
rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\RebalanceSlotsInGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@RebalanceSlotsInGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: RebalanceSlotsInGlobalReplicationGroup)

-- | If @True@, redistribution is applied immediately.
rebalanceSlotsInGlobalReplicationGroup_applyImmediately :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Core.Bool
rebalanceSlotsInGlobalReplicationGroup_applyImmediately = Lens.lens (\RebalanceSlotsInGlobalReplicationGroup' {applyImmediately} -> applyImmediately) (\s@RebalanceSlotsInGlobalReplicationGroup' {} a -> s {applyImmediately = a} :: RebalanceSlotsInGlobalReplicationGroup)

instance
  Core.AWSRequest
    RebalanceSlotsInGlobalReplicationGroup
  where
  type
    AWSResponse
      RebalanceSlotsInGlobalReplicationGroup =
      RebalanceSlotsInGlobalReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RebalanceSlotsInGlobalReplicationGroupResult"
      ( \s h x ->
          RebalanceSlotsInGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RebalanceSlotsInGlobalReplicationGroup

instance
  Core.NFData
    RebalanceSlotsInGlobalReplicationGroup

instance
  Core.ToHeaders
    RebalanceSlotsInGlobalReplicationGroup
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    RebalanceSlotsInGlobalReplicationGroup
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    RebalanceSlotsInGlobalReplicationGroup
  where
  toQuery RebalanceSlotsInGlobalReplicationGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "RebalanceSlotsInGlobalReplicationGroup" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId,
        "ApplyImmediately" Core.=: applyImmediately
      ]

-- | /See:/ 'newRebalanceSlotsInGlobalReplicationGroupResponse' smart constructor.
data RebalanceSlotsInGlobalReplicationGroupResponse = RebalanceSlotsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebalanceSlotsInGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newRebalanceSlotsInGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RebalanceSlotsInGlobalReplicationGroupResponse
newRebalanceSlotsInGlobalReplicationGroupResponse
  pHttpStatus_ =
    RebalanceSlotsInGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse (Core.Maybe GlobalReplicationGroup)
rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\RebalanceSlotsInGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@RebalanceSlotsInGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: RebalanceSlotsInGlobalReplicationGroupResponse)

-- | The response's http status code.
rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse Core.Int
rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus = Lens.lens (\RebalanceSlotsInGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@RebalanceSlotsInGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: RebalanceSlotsInGlobalReplicationGroupResponse)

instance
  Core.NFData
    RebalanceSlotsInGlobalReplicationGroupResponse
