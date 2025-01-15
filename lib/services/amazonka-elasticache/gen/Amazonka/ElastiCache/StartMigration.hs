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
-- Module      : Amazonka.ElastiCache.StartMigration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the migration of data.
module Amazonka.ElastiCache.StartMigration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
startMigration_customerNodeEndpointList = Lens.lens (\StartMigration' {customerNodeEndpointList} -> customerNodeEndpointList) (\s@StartMigration' {} a -> s {customerNodeEndpointList = a} :: StartMigration) Prelude.. Lens.coerced

instance Core.AWSRequest StartMigration where
  type
    AWSResponse StartMigration =
      StartMigrationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StartMigrationResult"
      ( \s h x ->
          StartMigrationResponse'
            Prelude.<$> (x Data..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMigration where
  hashWithSalt _salt StartMigration' {..} =
    _salt
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` customerNodeEndpointList

instance Prelude.NFData StartMigration where
  rnf StartMigration' {..} =
    Prelude.rnf replicationGroupId `Prelude.seq`
      Prelude.rnf customerNodeEndpointList

instance Data.ToHeaders StartMigration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartMigration where
  toPath = Prelude.const "/"

instance Data.ToQuery StartMigration where
  toQuery StartMigration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StartMigration" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "ReplicationGroupId" Data.=: replicationGroupId,
        "CustomerNodeEndpointList"
          Data.=: Data.toQueryList "member" customerNodeEndpointList
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

instance Prelude.NFData StartMigrationResponse where
  rnf StartMigrationResponse' {..} =
    Prelude.rnf replicationGroup `Prelude.seq`
      Prelude.rnf httpStatus
