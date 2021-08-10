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
-- Module      : Network.AWS.ElastiCache.StartMigration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the migration of data.
module Network.AWS.ElastiCache.StartMigration
  ( -- * Creating a Request
    StartMigration (..),
    newStartMigration,

    -- * Request Lenses
    startMigration_replicationGroupId,
    startMigration_customerNodeEndpointList,

    -- * Destructuring the Response
    StartMigrationResponse (..),
    newStartMigrationResponse,

    -- * Response Lenses
    startMigrationResponse_replicationGroup,
    startMigrationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartMigration' smart constructor.
data StartMigration = StartMigration'
  { -- | The ID of the replication group to which data should be migrated.
    replicationGroupId :: Prelude.Text,
    -- | List of endpoints from which data should be migrated. For Redis (cluster
    -- mode disabled), list should have only one element.
    customerNodeEndpointList :: [CustomerNodeEndpoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMigration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'startMigration_replicationGroupId' - The ID of the replication group to which data should be migrated.
--
-- 'customerNodeEndpointList', 'startMigration_customerNodeEndpointList' - List of endpoints from which data should be migrated. For Redis (cluster
-- mode disabled), list should have only one element.
newStartMigration ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  StartMigration
newStartMigration pReplicationGroupId_ =
  StartMigration'
    { replicationGroupId =
        pReplicationGroupId_,
      customerNodeEndpointList = Prelude.mempty
    }

-- | The ID of the replication group to which data should be migrated.
startMigration_replicationGroupId :: Lens.Lens' StartMigration Prelude.Text
startMigration_replicationGroupId = Lens.lens (\StartMigration' {replicationGroupId} -> replicationGroupId) (\s@StartMigration' {} a -> s {replicationGroupId = a} :: StartMigration)

-- | List of endpoints from which data should be migrated. For Redis (cluster
-- mode disabled), list should have only one element.
startMigration_customerNodeEndpointList :: Lens.Lens' StartMigration [CustomerNodeEndpoint]
startMigration_customerNodeEndpointList = Lens.lens (\StartMigration' {customerNodeEndpointList} -> customerNodeEndpointList) (\s@StartMigration' {} a -> s {customerNodeEndpointList = a} :: StartMigration) Prelude.. Lens._Coerce

instance Core.AWSRequest StartMigration where
  type
    AWSResponse StartMigration =
      StartMigrationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StartMigrationResult"
      ( \s h x ->
          StartMigrationResponse'
            Prelude.<$> (x Core..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMigration

instance Prelude.NFData StartMigration

instance Core.ToHeaders StartMigration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath StartMigration where
  toPath = Prelude.const "/"

instance Core.ToQuery StartMigration where
  toQuery StartMigration' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("StartMigration" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "CustomerNodeEndpointList"
          Core.=: Core.toQueryList "member" customerNodeEndpointList
      ]

-- | /See:/ 'newStartMigrationResponse' smart constructor.
data StartMigrationResponse = StartMigrationResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMigrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'startMigrationResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'startMigrationResponse_httpStatus' - The response's http status code.
newStartMigrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMigrationResponse
newStartMigrationResponse pHttpStatus_ =
  StartMigrationResponse'
    { replicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startMigrationResponse_replicationGroup :: Lens.Lens' StartMigrationResponse (Prelude.Maybe ReplicationGroup)
startMigrationResponse_replicationGroup = Lens.lens (\StartMigrationResponse' {replicationGroup} -> replicationGroup) (\s@StartMigrationResponse' {} a -> s {replicationGroup = a} :: StartMigrationResponse)

-- | The response's http status code.
startMigrationResponse_httpStatus :: Lens.Lens' StartMigrationResponse Prelude.Int
startMigrationResponse_httpStatus = Lens.lens (\StartMigrationResponse' {httpStatus} -> httpStatus) (\s@StartMigrationResponse' {} a -> s {httpStatus = a} :: StartMigrationResponse)

instance Prelude.NFData StartMigrationResponse
