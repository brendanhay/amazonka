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
-- Module      : Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to failover the primary region to a selected secondary region. The
-- selected secondary region will become primary, and all other clusters
-- will become secondary.
module Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
  ( -- * Creating a Request
    FailoverGlobalReplicationGroup (..),
    newFailoverGlobalReplicationGroup,

    -- * Request Lenses
    failoverGlobalReplicationGroup_globalReplicationGroupId,
    failoverGlobalReplicationGroup_primaryRegion,
    failoverGlobalReplicationGroup_primaryReplicationGroupId,

    -- * Destructuring the Response
    FailoverGlobalReplicationGroupResponse (..),
    newFailoverGlobalReplicationGroupResponse,

    -- * Response Lenses
    failoverGlobalReplicationGroupResponse_globalReplicationGroup,
    failoverGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newFailoverGlobalReplicationGroup' smart constructor.
data FailoverGlobalReplicationGroup = FailoverGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Text,
    -- | The AWS region of the primary cluster of the Global Datastore
    primaryRegion :: Core.Text,
    -- | The name of the primary replication group
    primaryReplicationGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailoverGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupId', 'failoverGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global Datastore
--
-- 'primaryRegion', 'failoverGlobalReplicationGroup_primaryRegion' - The AWS region of the primary cluster of the Global Datastore
--
-- 'primaryReplicationGroupId', 'failoverGlobalReplicationGroup_primaryReplicationGroupId' - The name of the primary replication group
newFailoverGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Core.Text ->
  -- | 'primaryRegion'
  Core.Text ->
  -- | 'primaryReplicationGroupId'
  Core.Text ->
  FailoverGlobalReplicationGroup
newFailoverGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pPrimaryRegion_
  pPrimaryReplicationGroupId_ =
    FailoverGlobalReplicationGroup'
      { globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        primaryRegion = pPrimaryRegion_,
        primaryReplicationGroupId =
          pPrimaryReplicationGroupId_
      }

-- | The name of the Global Datastore
failoverGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Core.Text
failoverGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\FailoverGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@FailoverGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: FailoverGlobalReplicationGroup)

-- | The AWS region of the primary cluster of the Global Datastore
failoverGlobalReplicationGroup_primaryRegion :: Lens.Lens' FailoverGlobalReplicationGroup Core.Text
failoverGlobalReplicationGroup_primaryRegion = Lens.lens (\FailoverGlobalReplicationGroup' {primaryRegion} -> primaryRegion) (\s@FailoverGlobalReplicationGroup' {} a -> s {primaryRegion = a} :: FailoverGlobalReplicationGroup)

-- | The name of the primary replication group
failoverGlobalReplicationGroup_primaryReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Core.Text
failoverGlobalReplicationGroup_primaryReplicationGroupId = Lens.lens (\FailoverGlobalReplicationGroup' {primaryReplicationGroupId} -> primaryReplicationGroupId) (\s@FailoverGlobalReplicationGroup' {} a -> s {primaryReplicationGroupId = a} :: FailoverGlobalReplicationGroup)

instance
  Core.AWSRequest
    FailoverGlobalReplicationGroup
  where
  type
    AWSResponse FailoverGlobalReplicationGroup =
      FailoverGlobalReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "FailoverGlobalReplicationGroupResult"
      ( \s h x ->
          FailoverGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable FailoverGlobalReplicationGroup

instance Core.NFData FailoverGlobalReplicationGroup

instance
  Core.ToHeaders
    FailoverGlobalReplicationGroup
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath FailoverGlobalReplicationGroup where
  toPath = Core.const "/"

instance Core.ToQuery FailoverGlobalReplicationGroup where
  toQuery FailoverGlobalReplicationGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "FailoverGlobalReplicationGroup" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId,
        "PrimaryRegion" Core.=: primaryRegion,
        "PrimaryReplicationGroupId"
          Core.=: primaryReplicationGroupId
      ]

-- | /See:/ 'newFailoverGlobalReplicationGroupResponse' smart constructor.
data FailoverGlobalReplicationGroupResponse = FailoverGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailoverGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'failoverGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'failoverGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newFailoverGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  FailoverGlobalReplicationGroupResponse
newFailoverGlobalReplicationGroupResponse
  pHttpStatus_ =
    FailoverGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
failoverGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' FailoverGlobalReplicationGroupResponse (Core.Maybe GlobalReplicationGroup)
failoverGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\FailoverGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@FailoverGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: FailoverGlobalReplicationGroupResponse)

-- | The response's http status code.
failoverGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' FailoverGlobalReplicationGroupResponse Core.Int
failoverGlobalReplicationGroupResponse_httpStatus = Lens.lens (\FailoverGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@FailoverGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: FailoverGlobalReplicationGroupResponse)

instance
  Core.NFData
    FailoverGlobalReplicationGroupResponse
