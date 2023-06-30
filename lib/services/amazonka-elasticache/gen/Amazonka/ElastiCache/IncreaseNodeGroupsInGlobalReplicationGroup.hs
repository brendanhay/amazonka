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
-- Module      : Amazonka.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increase the number of node groups in the Global datastore
module Amazonka.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a Request
    IncreaseNodeGroupsInGlobalReplicationGroup (..),
    newIncreaseNodeGroupsInGlobalReplicationGroup,

    -- * Request Lenses
    increaseNodeGroupsInGlobalReplicationGroup_regionalConfigurations,
    increaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId,
    increaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount,
    increaseNodeGroupsInGlobalReplicationGroup_applyImmediately,

    -- * Destructuring the Response
    IncreaseNodeGroupsInGlobalReplicationGroupResponse (..),
    newIncreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- * Response Lenses
    increaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup,
    increaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newIncreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroup = IncreaseNodeGroupsInGlobalReplicationGroup'
  { -- | Describes the replication group IDs, the Amazon regions where they are
    -- stored and the shard configuration for each that comprise the Global
    -- datastore
    regionalConfigurations :: Prelude.Maybe [RegionalConfiguration],
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | The number of node groups you wish to add
    nodeGroupCount :: Prelude.Int,
    -- | Indicates that the process begins immediately. At present, the only
    -- permitted value for this parameter is true.
    applyImmediately :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncreaseNodeGroupsInGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionalConfigurations', 'increaseNodeGroupsInGlobalReplicationGroup_regionalConfigurations' - Describes the replication group IDs, the Amazon regions where they are
-- stored and the shard configuration for each that comprise the Global
-- datastore
--
-- 'globalReplicationGroupId', 'increaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
--
-- 'nodeGroupCount', 'increaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount' - The number of node groups you wish to add
--
-- 'applyImmediately', 'increaseNodeGroupsInGlobalReplicationGroup_applyImmediately' - Indicates that the process begins immediately. At present, the only
-- permitted value for this parameter is true.
newIncreaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Prelude.Text ->
  -- | 'nodeGroupCount'
  Prelude.Int ->
  -- | 'applyImmediately'
  Prelude.Bool ->
  IncreaseNodeGroupsInGlobalReplicationGroup
newIncreaseNodeGroupsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pNodeGroupCount_
  pApplyImmediately_ =
    IncreaseNodeGroupsInGlobalReplicationGroup'
      { regionalConfigurations =
          Prelude.Nothing,
        globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        nodeGroupCount =
          pNodeGroupCount_,
        applyImmediately =
          pApplyImmediately_
      }

-- | Describes the replication group IDs, the Amazon regions where they are
-- stored and the shard configuration for each that comprise the Global
-- datastore
increaseNodeGroupsInGlobalReplicationGroup_regionalConfigurations :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup (Prelude.Maybe [RegionalConfiguration])
increaseNodeGroupsInGlobalReplicationGroup_regionalConfigurations = Lens.lens (\IncreaseNodeGroupsInGlobalReplicationGroup' {regionalConfigurations} -> regionalConfigurations) (\s@IncreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {regionalConfigurations = a} :: IncreaseNodeGroupsInGlobalReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Global datastore
increaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Prelude.Text
increaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\IncreaseNodeGroupsInGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@IncreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: IncreaseNodeGroupsInGlobalReplicationGroup)

-- | The number of node groups you wish to add
increaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Prelude.Int
increaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount = Lens.lens (\IncreaseNodeGroupsInGlobalReplicationGroup' {nodeGroupCount} -> nodeGroupCount) (\s@IncreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {nodeGroupCount = a} :: IncreaseNodeGroupsInGlobalReplicationGroup)

-- | Indicates that the process begins immediately. At present, the only
-- permitted value for this parameter is true.
increaseNodeGroupsInGlobalReplicationGroup_applyImmediately :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Prelude.Bool
increaseNodeGroupsInGlobalReplicationGroup_applyImmediately = Lens.lens (\IncreaseNodeGroupsInGlobalReplicationGroup' {applyImmediately} -> applyImmediately) (\s@IncreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {applyImmediately = a} :: IncreaseNodeGroupsInGlobalReplicationGroup)

instance
  Core.AWSRequest
    IncreaseNodeGroupsInGlobalReplicationGroup
  where
  type
    AWSResponse
      IncreaseNodeGroupsInGlobalReplicationGroup =
      IncreaseNodeGroupsInGlobalReplicationGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "IncreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          IncreaseNodeGroupsInGlobalReplicationGroupResponse'
            Prelude.<$> (x Data..@? "GlobalReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    IncreaseNodeGroupsInGlobalReplicationGroup
  where
  hashWithSalt
    _salt
    IncreaseNodeGroupsInGlobalReplicationGroup' {..} =
      _salt
        `Prelude.hashWithSalt` regionalConfigurations
        `Prelude.hashWithSalt` globalReplicationGroupId
        `Prelude.hashWithSalt` nodeGroupCount
        `Prelude.hashWithSalt` applyImmediately

instance
  Prelude.NFData
    IncreaseNodeGroupsInGlobalReplicationGroup
  where
  rnf IncreaseNodeGroupsInGlobalReplicationGroup' {..} =
    Prelude.rnf regionalConfigurations
      `Prelude.seq` Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf nodeGroupCount
      `Prelude.seq` Prelude.rnf applyImmediately

instance
  Data.ToHeaders
    IncreaseNodeGroupsInGlobalReplicationGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    IncreaseNodeGroupsInGlobalReplicationGroup
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    IncreaseNodeGroupsInGlobalReplicationGroup
  where
  toQuery
    IncreaseNodeGroupsInGlobalReplicationGroup' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "IncreaseNodeGroupsInGlobalReplicationGroup" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2015-02-02" :: Prelude.ByteString),
          "RegionalConfigurations"
            Data.=: Data.toQuery
              ( Data.toQueryList "RegionalConfiguration"
                  Prelude.<$> regionalConfigurations
              ),
          "GlobalReplicationGroupId"
            Data.=: globalReplicationGroupId,
          "NodeGroupCount" Data.=: nodeGroupCount,
          "ApplyImmediately" Data.=: applyImmediately
        ]

-- | /See:/ 'newIncreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroupResponse = IncreaseNodeGroupsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncreaseNodeGroupsInGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'increaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'increaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newIncreaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  IncreaseNodeGroupsInGlobalReplicationGroupResponse
newIncreaseNodeGroupsInGlobalReplicationGroupResponse
  pHttpStatus_ =
    IncreaseNodeGroupsInGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
increaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
increaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\IncreaseNodeGroupsInGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@IncreaseNodeGroupsInGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: IncreaseNodeGroupsInGlobalReplicationGroupResponse)

-- | The response's http status code.
increaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse Prelude.Int
increaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus = Lens.lens (\IncreaseNodeGroupsInGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@IncreaseNodeGroupsInGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: IncreaseNodeGroupsInGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    IncreaseNodeGroupsInGlobalReplicationGroupResponse
  where
  rnf
    IncreaseNodeGroupsInGlobalReplicationGroupResponse' {..} =
      Prelude.rnf globalReplicationGroup
        `Prelude.seq` Prelude.rnf httpStatus
