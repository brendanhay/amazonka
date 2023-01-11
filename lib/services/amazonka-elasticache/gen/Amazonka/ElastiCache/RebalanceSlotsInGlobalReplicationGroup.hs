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
-- Module      : Amazonka.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redistribute slots to ensure uniform distribution across existing shards
-- in the cluster.
module Amazonka.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRebalanceSlotsInGlobalReplicationGroup' smart constructor.
data RebalanceSlotsInGlobalReplicationGroup = RebalanceSlotsInGlobalReplicationGroup'
  { -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | If @True@, redistribution is applied immediately.
    applyImmediately :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebalanceSlotsInGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupId', 'rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
--
-- 'applyImmediately', 'rebalanceSlotsInGlobalReplicationGroup_applyImmediately' - If @True@, redistribution is applied immediately.
newRebalanceSlotsInGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Prelude.Text ->
  -- | 'applyImmediately'
  Prelude.Bool ->
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

-- | The name of the Global datastore
rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Prelude.Text
rebalanceSlotsInGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\RebalanceSlotsInGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@RebalanceSlotsInGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: RebalanceSlotsInGlobalReplicationGroup)

-- | If @True@, redistribution is applied immediately.
rebalanceSlotsInGlobalReplicationGroup_applyImmediately :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Prelude.Bool
rebalanceSlotsInGlobalReplicationGroup_applyImmediately = Lens.lens (\RebalanceSlotsInGlobalReplicationGroup' {applyImmediately} -> applyImmediately) (\s@RebalanceSlotsInGlobalReplicationGroup' {} a -> s {applyImmediately = a} :: RebalanceSlotsInGlobalReplicationGroup)

instance
  Core.AWSRequest
    RebalanceSlotsInGlobalReplicationGroup
  where
  type
    AWSResponse
      RebalanceSlotsInGlobalReplicationGroup =
      RebalanceSlotsInGlobalReplicationGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RebalanceSlotsInGlobalReplicationGroupResult"
      ( \s h x ->
          RebalanceSlotsInGlobalReplicationGroupResponse'
            Prelude.<$> (x Data..@? "GlobalReplicationGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RebalanceSlotsInGlobalReplicationGroup
  where
  hashWithSalt
    _salt
    RebalanceSlotsInGlobalReplicationGroup' {..} =
      _salt
        `Prelude.hashWithSalt` globalReplicationGroupId
        `Prelude.hashWithSalt` applyImmediately

instance
  Prelude.NFData
    RebalanceSlotsInGlobalReplicationGroup
  where
  rnf RebalanceSlotsInGlobalReplicationGroup' {..} =
    Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf applyImmediately

instance
  Data.ToHeaders
    RebalanceSlotsInGlobalReplicationGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    RebalanceSlotsInGlobalReplicationGroup
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RebalanceSlotsInGlobalReplicationGroup
  where
  toQuery RebalanceSlotsInGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RebalanceSlotsInGlobalReplicationGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "GlobalReplicationGroupId"
          Data.=: globalReplicationGroupId,
        "ApplyImmediately" Data.=: applyImmediately
      ]

-- | /See:/ 'newRebalanceSlotsInGlobalReplicationGroupResponse' smart constructor.
data RebalanceSlotsInGlobalReplicationGroupResponse = RebalanceSlotsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RebalanceSlotsInGlobalReplicationGroupResponse
newRebalanceSlotsInGlobalReplicationGroupResponse
  pHttpStatus_ =
    RebalanceSlotsInGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
rebalanceSlotsInGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\RebalanceSlotsInGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@RebalanceSlotsInGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: RebalanceSlotsInGlobalReplicationGroupResponse)

-- | The response's http status code.
rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse Prelude.Int
rebalanceSlotsInGlobalReplicationGroupResponse_httpStatus = Lens.lens (\RebalanceSlotsInGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@RebalanceSlotsInGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: RebalanceSlotsInGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    RebalanceSlotsInGlobalReplicationGroupResponse
  where
  rnf
    RebalanceSlotsInGlobalReplicationGroupResponse' {..} =
      Prelude.rnf globalReplicationGroup
        `Prelude.seq` Prelude.rnf httpStatus
