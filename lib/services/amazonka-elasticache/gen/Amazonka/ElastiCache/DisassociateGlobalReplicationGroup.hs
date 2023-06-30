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
-- Module      : Amazonka.ElastiCache.DisassociateGlobalReplicationGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a secondary cluster from the Global datastore using the Global
-- datastore name. The secondary cluster will no longer receive updates
-- from the primary cluster, but will remain as a standalone cluster in
-- that Amazon region.
module Amazonka.ElastiCache.DisassociateGlobalReplicationGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateGlobalReplicationGroup' smart constructor.
data DisassociateGlobalReplicationGroup = DisassociateGlobalReplicationGroup'
  { -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | The name of the secondary cluster you wish to remove from the Global
    -- datastore
    replicationGroupId :: Prelude.Text,
    -- | The Amazon region of secondary cluster you wish to remove from the
    -- Global datastore
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
-- 'globalReplicationGroupId', 'disassociateGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
--
-- 'replicationGroupId', 'disassociateGlobalReplicationGroup_replicationGroupId' - The name of the secondary cluster you wish to remove from the Global
-- datastore
--
-- 'replicationGroupRegion', 'disassociateGlobalReplicationGroup_replicationGroupRegion' - The Amazon region of secondary cluster you wish to remove from the
-- Global datastore
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

-- | The name of the Global datastore
disassociateGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Prelude.Text
disassociateGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\DisassociateGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DisassociateGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: DisassociateGlobalReplicationGroup)

-- | The name of the secondary cluster you wish to remove from the Global
-- datastore
disassociateGlobalReplicationGroup_replicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Prelude.Text
disassociateGlobalReplicationGroup_replicationGroupId = Lens.lens (\DisassociateGlobalReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@DisassociateGlobalReplicationGroup' {} a -> s {replicationGroupId = a} :: DisassociateGlobalReplicationGroup)

-- | The Amazon region of secondary cluster you wish to remove from the
-- Global datastore
disassociateGlobalReplicationGroup_replicationGroupRegion :: Lens.Lens' DisassociateGlobalReplicationGroup Prelude.Text
disassociateGlobalReplicationGroup_replicationGroupRegion = Lens.lens (\DisassociateGlobalReplicationGroup' {replicationGroupRegion} -> replicationGroupRegion) (\s@DisassociateGlobalReplicationGroup' {} a -> s {replicationGroupRegion = a} :: DisassociateGlobalReplicationGroup)

instance
  Core.AWSRequest
    DisassociateGlobalReplicationGroup
  where
  type
    AWSResponse DisassociateGlobalReplicationGroup =
      DisassociateGlobalReplicationGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DisassociateGlobalReplicationGroupResult"
      ( \s h x ->
          DisassociateGlobalReplicationGroupResponse'
            Prelude.<$> (x Data..@? "GlobalReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateGlobalReplicationGroup
  where
  hashWithSalt
    _salt
    DisassociateGlobalReplicationGroup' {..} =
      _salt
        `Prelude.hashWithSalt` globalReplicationGroupId
        `Prelude.hashWithSalt` replicationGroupId
        `Prelude.hashWithSalt` replicationGroupRegion

instance
  Prelude.NFData
    DisassociateGlobalReplicationGroup
  where
  rnf DisassociateGlobalReplicationGroup' {..} =
    Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf replicationGroupId
      `Prelude.seq` Prelude.rnf replicationGroupRegion

instance
  Data.ToHeaders
    DisassociateGlobalReplicationGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisassociateGlobalReplicationGroup
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateGlobalReplicationGroup
  where
  toQuery DisassociateGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisassociateGlobalReplicationGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "GlobalReplicationGroupId"
          Data.=: globalReplicationGroupId,
        "ReplicationGroupId" Data.=: replicationGroupId,
        "ReplicationGroupRegion"
          Data.=: replicationGroupRegion
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
  where
  rnf DisassociateGlobalReplicationGroupResponse' {..} =
    Prelude.rnf globalReplicationGroup
      `Prelude.seq` Prelude.rnf httpStatus
