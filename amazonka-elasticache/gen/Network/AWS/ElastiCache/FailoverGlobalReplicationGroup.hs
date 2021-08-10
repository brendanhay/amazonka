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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newFailoverGlobalReplicationGroup' smart constructor.
data FailoverGlobalReplicationGroup = FailoverGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | The AWS region of the primary cluster of the Global Datastore
    primaryRegion :: Prelude.Text,
    -- | The name of the primary replication group
    primaryReplicationGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'primaryRegion'
  Prelude.Text ->
  -- | 'primaryReplicationGroupId'
  Prelude.Text ->
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
failoverGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Prelude.Text
failoverGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\FailoverGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@FailoverGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: FailoverGlobalReplicationGroup)

-- | The AWS region of the primary cluster of the Global Datastore
failoverGlobalReplicationGroup_primaryRegion :: Lens.Lens' FailoverGlobalReplicationGroup Prelude.Text
failoverGlobalReplicationGroup_primaryRegion = Lens.lens (\FailoverGlobalReplicationGroup' {primaryRegion} -> primaryRegion) (\s@FailoverGlobalReplicationGroup' {} a -> s {primaryRegion = a} :: FailoverGlobalReplicationGroup)

-- | The name of the primary replication group
failoverGlobalReplicationGroup_primaryReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Prelude.Text
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
            Prelude.<$> (x Core..@? "GlobalReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    FailoverGlobalReplicationGroup

instance
  Prelude.NFData
    FailoverGlobalReplicationGroup

instance
  Core.ToHeaders
    FailoverGlobalReplicationGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath FailoverGlobalReplicationGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery FailoverGlobalReplicationGroup where
  toQuery FailoverGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "FailoverGlobalReplicationGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId,
        "PrimaryRegion" Core.=: primaryRegion,
        "PrimaryReplicationGroupId"
          Core.=: primaryReplicationGroupId
      ]

-- | /See:/ 'newFailoverGlobalReplicationGroupResponse' smart constructor.
data FailoverGlobalReplicationGroupResponse = FailoverGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  FailoverGlobalReplicationGroupResponse
newFailoverGlobalReplicationGroupResponse
  pHttpStatus_ =
    FailoverGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
failoverGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' FailoverGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
failoverGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\FailoverGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@FailoverGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: FailoverGlobalReplicationGroupResponse)

-- | The response's http status code.
failoverGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' FailoverGlobalReplicationGroupResponse Prelude.Int
failoverGlobalReplicationGroupResponse_httpStatus = Lens.lens (\FailoverGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@FailoverGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: FailoverGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    FailoverGlobalReplicationGroupResponse
