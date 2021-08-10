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
-- Module      : Network.AWS.Kinesis.DescribeLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Kinesis.DescribeLimits
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
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLimitsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ShardLimit")
            Prelude.<*> (x Core..:> "OpenShardCount")
      )

instance Prelude.Hashable DescribeLimits

instance Prelude.NFData DescribeLimits

instance Core.ToHeaders DescribeLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.DescribeLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLimits where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeLimits where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLimits where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The maximum number of shards.
    shardLimit :: Prelude.Natural,
    -- | The number of open shards.
    openShardCount :: Prelude.Natural
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
newDescribeLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'shardLimit'
  Prelude.Natural ->
  -- | 'openShardCount'
  Prelude.Natural ->
  DescribeLimitsResponse
newDescribeLimitsResponse
  pHttpStatus_
  pShardLimit_
  pOpenShardCount_ =
    DescribeLimitsResponse'
      { httpStatus = pHttpStatus_,
        shardLimit = pShardLimit_,
        openShardCount = pOpenShardCount_
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

instance Prelude.NFData DescribeLimitsResponse
