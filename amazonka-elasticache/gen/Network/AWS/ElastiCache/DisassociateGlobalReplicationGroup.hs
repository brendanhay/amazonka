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
-- Module      : Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a secondary cluster from the Global Datastore using the Global
-- Datastore name. The secondary cluster will no longer receive updates
-- from the primary cluster, but will remain as a standalone cluster in
-- that AWS region.
module Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
  ( -- * Creating a Request
    DisassociateGlobalReplicationGroup (..),
    newDisassociateGlobalReplicationGroup,

    -- * Request Lenses
    disassociateGlobalReplicationGroup_globalReplicationGroupId,
    disassociateGlobalReplicationGroup_replicationGroupId,
    disassociateGlobalReplicationGroup_replicationGroupRegion,

    -- * Destructuring the Response
    DisassociateGlobalReplicationGroupResponse (..),
    newDisassociateGlobalReplicationGroupResponse,

    -- * Response Lenses
    disassociateGlobalReplicationGroupResponse_globalReplicationGroup,
    disassociateGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateGlobalReplicationGroup' smart constructor.
data DisassociateGlobalReplicationGroup = DisassociateGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | The name of the secondary cluster you wish to remove from the Global
    -- Datastore
    replicationGroupId :: Prelude.Text,
    -- | The AWS region of secondary cluster you wish to remove from the Global
    -- Datastore
    replicationGroupRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupId', 'disassociateGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global Datastore
--
-- 'replicationGroupId', 'disassociateGlobalReplicationGroup_replicationGroupId' - The name of the secondary cluster you wish to remove from the Global
-- Datastore
--
-- 'replicationGroupRegion', 'disassociateGlobalReplicationGroup_replicationGroupRegion' - The AWS region of secondary cluster you wish to remove from the Global
-- Datastore
newDisassociateGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Prelude.Text ->
  -- | 'replicationGroupId'
  Prelude.Text ->
  -- | 'replicationGroupRegion'
  Prelude.Text ->
  DisassociateGlobalReplicationGroup
newDisassociateGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pReplicationGroupId_
  pReplicationGroupRegion_ =
    DisassociateGlobalReplicationGroup'
      { globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        replicationGroupId =
          pReplicationGroupId_,
        replicationGroupRegion =
          pReplicationGroupRegion_
      }

-- | The name of the Global Datastore
disassociateGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Prelude.Text
disassociateGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\DisassociateGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DisassociateGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: DisassociateGlobalReplicationGroup)

-- | The name of the secondary cluster you wish to remove from the Global
-- Datastore
disassociateGlobalReplicationGroup_replicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Prelude.Text
disassociateGlobalReplicationGroup_replicationGroupId = Lens.lens (\DisassociateGlobalReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@DisassociateGlobalReplicationGroup' {} a -> s {replicationGroupId = a} :: DisassociateGlobalReplicationGroup)

-- | The AWS region of secondary cluster you wish to remove from the Global
-- Datastore
disassociateGlobalReplicationGroup_replicationGroupRegion :: Lens.Lens' DisassociateGlobalReplicationGroup Prelude.Text
disassociateGlobalReplicationGroup_replicationGroupRegion = Lens.lens (\DisassociateGlobalReplicationGroup' {replicationGroupRegion} -> replicationGroupRegion) (\s@DisassociateGlobalReplicationGroup' {} a -> s {replicationGroupRegion = a} :: DisassociateGlobalReplicationGroup)

instance
  Core.AWSRequest
    DisassociateGlobalReplicationGroup
  where
  type
    AWSResponse DisassociateGlobalReplicationGroup =
      DisassociateGlobalReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DisassociateGlobalReplicationGroupResult"
      ( \s h x ->
          DisassociateGlobalReplicationGroupResponse'
            Prelude.<$> (x Core..@? "GlobalReplicationGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateGlobalReplicationGroup

instance
  Prelude.NFData
    DisassociateGlobalReplicationGroup

instance
  Core.ToHeaders
    DisassociateGlobalReplicationGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DisassociateGlobalReplicationGroup
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DisassociateGlobalReplicationGroup
  where
  toQuery DisassociateGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DisassociateGlobalReplicationGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId,
        "ReplicationGroupId" Core.=: replicationGroupId,
        "ReplicationGroupRegion"
          Core.=: replicationGroupRegion
      ]

-- | /See:/ 'newDisassociateGlobalReplicationGroupResponse' smart constructor.
data DisassociateGlobalReplicationGroupResponse = DisassociateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'disassociateGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'disassociateGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newDisassociateGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateGlobalReplicationGroupResponse
newDisassociateGlobalReplicationGroupResponse
  pHttpStatus_ =
    DisassociateGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
disassociateGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' DisassociateGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
disassociateGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\DisassociateGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@DisassociateGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: DisassociateGlobalReplicationGroupResponse)

-- | The response's http status code.
disassociateGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' DisassociateGlobalReplicationGroupResponse Prelude.Int
disassociateGlobalReplicationGroupResponse_httpStatus = Lens.lens (\DisassociateGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: DisassociateGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    DisassociateGlobalReplicationGroupResponse
