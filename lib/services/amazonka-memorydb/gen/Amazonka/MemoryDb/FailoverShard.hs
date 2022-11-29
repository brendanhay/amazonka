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
-- Module      : Amazonka.MemoryDb.FailoverShard
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to failover a shard. This API is designed for testing the behavior
-- of your application in case of MemoryDB failover. It is not designed to
-- be used as a production-level tool for initiating a failover to overcome
-- a problem you may have with the cluster. Moreover, in certain conditions
-- such as large scale operational events, Amazon may block this API.
module Amazonka.MemoryDb.FailoverShard
  ( -- * Creating a Request
    FailoverShard (..),
    newFailoverShard,

    -- * Request Lenses
    failoverShard_clusterName,
    failoverShard_shardName,

    -- * Destructuring the Response
    FailoverShardResponse (..),
    newFailoverShardResponse,

    -- * Response Lenses
    failoverShardResponse_cluster,
    failoverShardResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newFailoverShard' smart constructor.
data FailoverShard = FailoverShard'
  { -- | The cluster being failed over
    clusterName :: Prelude.Text,
    -- | The name of the shard
    shardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverShard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'failoverShard_clusterName' - The cluster being failed over
--
-- 'shardName', 'failoverShard_shardName' - The name of the shard
newFailoverShard ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'shardName'
  Prelude.Text ->
  FailoverShard
newFailoverShard pClusterName_ pShardName_ =
  FailoverShard'
    { clusterName = pClusterName_,
      shardName = pShardName_
    }

-- | The cluster being failed over
failoverShard_clusterName :: Lens.Lens' FailoverShard Prelude.Text
failoverShard_clusterName = Lens.lens (\FailoverShard' {clusterName} -> clusterName) (\s@FailoverShard' {} a -> s {clusterName = a} :: FailoverShard)

-- | The name of the shard
failoverShard_shardName :: Lens.Lens' FailoverShard Prelude.Text
failoverShard_shardName = Lens.lens (\FailoverShard' {shardName} -> shardName) (\s@FailoverShard' {} a -> s {shardName = a} :: FailoverShard)

instance Core.AWSRequest FailoverShard where
  type
    AWSResponse FailoverShard =
      FailoverShardResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          FailoverShardResponse'
            Prelude.<$> (x Core..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FailoverShard where
  hashWithSalt _salt FailoverShard' {..} =
    _salt `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` shardName

instance Prelude.NFData FailoverShard where
  rnf FailoverShard' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf shardName

instance Core.ToHeaders FailoverShard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.FailoverShard" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON FailoverShard where
  toJSON FailoverShard' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterName" Core..= clusterName),
            Prelude.Just ("ShardName" Core..= shardName)
          ]
      )

instance Core.ToPath FailoverShard where
  toPath = Prelude.const "/"

instance Core.ToQuery FailoverShard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newFailoverShardResponse' smart constructor.
data FailoverShardResponse = FailoverShardResponse'
  { -- | The cluster being failed over
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverShardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'failoverShardResponse_cluster' - The cluster being failed over
--
-- 'httpStatus', 'failoverShardResponse_httpStatus' - The response's http status code.
newFailoverShardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  FailoverShardResponse
newFailoverShardResponse pHttpStatus_ =
  FailoverShardResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cluster being failed over
failoverShardResponse_cluster :: Lens.Lens' FailoverShardResponse (Prelude.Maybe Cluster)
failoverShardResponse_cluster = Lens.lens (\FailoverShardResponse' {cluster} -> cluster) (\s@FailoverShardResponse' {} a -> s {cluster = a} :: FailoverShardResponse)

-- | The response's http status code.
failoverShardResponse_httpStatus :: Lens.Lens' FailoverShardResponse Prelude.Int
failoverShardResponse_httpStatus = Lens.lens (\FailoverShardResponse' {httpStatus} -> httpStatus) (\s@FailoverShardResponse' {} a -> s {httpStatus = a} :: FailoverShardResponse)

instance Prelude.NFData FailoverShardResponse where
  rnf FailoverShardResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
