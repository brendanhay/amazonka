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
-- Module      : Amazonka.Kinesis.DescribeLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the shard limits and usage for the account.
--
-- If you update your account limits, the old limits might be returned for
-- a few minutes.
--
-- This operation has a limit of one transaction per second per account.
module Amazonka.Kinesis.DescribeLimits
  ( -- * Creating a Request
    DescribeLimits (..),
    newDescribeLimits,

    -- * Destructuring the Response
    DescribeLimitsResponse (..),
    newDescribeLimitsResponse,

    -- * Response Lenses
    describeLimitsResponse_httpStatus,
    describeLimitsResponse_shardLimit,
    describeLimitsResponse_openShardCount,
    describeLimitsResponse_onDemandStreamCount,
    describeLimitsResponse_onDemandStreamCountLimit,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLimits' smart constructor.
data DescribeLimits = DescribeLimits'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeLimits ::
  DescribeLimits
newDescribeLimits = DescribeLimits'

instance Core.AWSRequest DescribeLimits where
  type
    AWSResponse DescribeLimits =
      DescribeLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLimitsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ShardLimit")
            Prelude.<*> (x Data..:> "OpenShardCount")
            Prelude.<*> (x Data..:> "OnDemandStreamCount")
            Prelude.<*> (x Data..:> "OnDemandStreamCountLimit")
      )

instance Prelude.Hashable DescribeLimits where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeLimits where
  rnf _ = ()

instance Data.ToHeaders DescribeLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.DescribeLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLimits where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLimits where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The maximum number of shards.
    shardLimit :: Prelude.Natural,
    -- | The number of open shards.
    openShardCount :: Prelude.Natural,
    -- | Indicates the number of data streams with the on-demand capacity mode.
    onDemandStreamCount :: Prelude.Natural,
    -- | The maximum number of data streams with the on-demand capacity mode.
    onDemandStreamCountLimit :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeLimitsResponse_httpStatus' - The response's http status code.
--
-- 'shardLimit', 'describeLimitsResponse_shardLimit' - The maximum number of shards.
--
-- 'openShardCount', 'describeLimitsResponse_openShardCount' - The number of open shards.
--
-- 'onDemandStreamCount', 'describeLimitsResponse_onDemandStreamCount' - Indicates the number of data streams with the on-demand capacity mode.
--
-- 'onDemandStreamCountLimit', 'describeLimitsResponse_onDemandStreamCountLimit' - The maximum number of data streams with the on-demand capacity mode.
newDescribeLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'shardLimit'
  Prelude.Natural ->
  -- | 'openShardCount'
  Prelude.Natural ->
  -- | 'onDemandStreamCount'
  Prelude.Natural ->
  -- | 'onDemandStreamCountLimit'
  Prelude.Natural ->
  DescribeLimitsResponse
newDescribeLimitsResponse
  pHttpStatus_
  pShardLimit_
  pOpenShardCount_
  pOnDemandStreamCount_
  pOnDemandStreamCountLimit_ =
    DescribeLimitsResponse'
      { httpStatus = pHttpStatus_,
        shardLimit = pShardLimit_,
        openShardCount = pOpenShardCount_,
        onDemandStreamCount = pOnDemandStreamCount_,
        onDemandStreamCountLimit =
          pOnDemandStreamCountLimit_
      }

-- | The response's http status code.
describeLimitsResponse_httpStatus :: Lens.Lens' DescribeLimitsResponse Prelude.Int
describeLimitsResponse_httpStatus = Lens.lens (\DescribeLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeLimitsResponse' {} a -> s {httpStatus = a} :: DescribeLimitsResponse)

-- | The maximum number of shards.
describeLimitsResponse_shardLimit :: Lens.Lens' DescribeLimitsResponse Prelude.Natural
describeLimitsResponse_shardLimit = Lens.lens (\DescribeLimitsResponse' {shardLimit} -> shardLimit) (\s@DescribeLimitsResponse' {} a -> s {shardLimit = a} :: DescribeLimitsResponse)

-- | The number of open shards.
describeLimitsResponse_openShardCount :: Lens.Lens' DescribeLimitsResponse Prelude.Natural
describeLimitsResponse_openShardCount = Lens.lens (\DescribeLimitsResponse' {openShardCount} -> openShardCount) (\s@DescribeLimitsResponse' {} a -> s {openShardCount = a} :: DescribeLimitsResponse)

-- | Indicates the number of data streams with the on-demand capacity mode.
describeLimitsResponse_onDemandStreamCount :: Lens.Lens' DescribeLimitsResponse Prelude.Natural
describeLimitsResponse_onDemandStreamCount = Lens.lens (\DescribeLimitsResponse' {onDemandStreamCount} -> onDemandStreamCount) (\s@DescribeLimitsResponse' {} a -> s {onDemandStreamCount = a} :: DescribeLimitsResponse)

-- | The maximum number of data streams with the on-demand capacity mode.
describeLimitsResponse_onDemandStreamCountLimit :: Lens.Lens' DescribeLimitsResponse Prelude.Natural
describeLimitsResponse_onDemandStreamCountLimit = Lens.lens (\DescribeLimitsResponse' {onDemandStreamCountLimit} -> onDemandStreamCountLimit) (\s@DescribeLimitsResponse' {} a -> s {onDemandStreamCountLimit = a} :: DescribeLimitsResponse)

instance Prelude.NFData DescribeLimitsResponse where
  rnf DescribeLimitsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf shardLimit
      `Prelude.seq` Prelude.rnf openShardCount
      `Prelude.seq` Prelude.rnf onDemandStreamCount
      `Prelude.seq` Prelude.rnf onDemandStreamCountLimit
