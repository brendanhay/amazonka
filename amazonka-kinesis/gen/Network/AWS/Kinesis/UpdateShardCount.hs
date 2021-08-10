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
-- Module      : Network.AWS.Kinesis.UpdateShardCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shard count of the specified stream to the specified number
-- of shards.
--
-- Updating the shard count is an asynchronous operation. Upon receiving
-- the request, Kinesis Data Streams returns immediately and sets the
-- status of the stream to @UPDATING@. After the update is complete,
-- Kinesis Data Streams sets the status of the stream back to @ACTIVE@.
-- Depending on the size of the stream, the scaling action could take a few
-- minutes to complete. You can continue to read and write data to your
-- stream while its status is @UPDATING@.
--
-- To update the shard count, Kinesis Data Streams performs splits or
-- merges on individual shards. This can cause short-lived shards to be
-- created, in addition to the final shards. These short-lived shards count
-- towards your total shard limit for your account in the Region.
--
-- When using this operation, we recommend that you specify a target shard
-- count that is a multiple of 25% (25%, 50%, 75%, 100%). You can specify
-- any target value within your shard limit. However, if you specify a
-- target that isn\'t a multiple of 25%, the scaling action might take
-- longer to complete.
--
-- This operation has the following default limits. By default, you cannot
-- do the following:
--
-- -   Scale more than ten times per rolling 24-hour period per stream
--
-- -   Scale up to more than double your current shard count for a stream
--
-- -   Scale down below half your current shard count for a stream
--
-- -   Scale up to more than 500 shards in a stream
--
-- -   Scale a stream with more than 500 shards down unless the result is
--     less than 500 shards
--
-- -   Scale up to more than the shard limit for your account
--
-- For the default limits for an AWS account, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/. To request an
-- increase in the call rate limit, the shard limit for this API, or your
-- overall shard limit, use the
-- <https://console.aws.amazon.com/support/v1#/case/create?issueType=service-limit-increase&limitType=service-code-kinesis limits form>.
module Network.AWS.Kinesis.UpdateShardCount
  ( -- * Creating a Request
    UpdateShardCount (..),
    newUpdateShardCount,

    -- * Request Lenses
    updateShardCount_streamName,
    updateShardCount_targetShardCount,
    updateShardCount_scalingType,

    -- * Destructuring the Response
    UpdateShardCountResponse (..),
    newUpdateShardCountResponse,

    -- * Response Lenses
    updateShardCountResponse_targetShardCount,
    updateShardCountResponse_currentShardCount,
    updateShardCountResponse_streamName,
    updateShardCountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateShardCount' smart constructor.
data UpdateShardCount = UpdateShardCount'
  { -- | The name of the stream.
    streamName :: Prelude.Text,
    -- | The new number of shards. This value has the following default limits.
    -- By default, you cannot do the following:
    --
    -- -   Set this value to more than double your current shard count for a
    --     stream.
    --
    -- -   Set this value below half your current shard count for a stream.
    --
    -- -   Set this value to more than 500 shards in a stream (the default
    --     limit for shard count per stream is 500 per account per region),
    --     unless you request a limit increase.
    --
    -- -   Scale a stream with more than 500 shards down unless you set this
    --     value to less than 500 shards.
    targetShardCount :: Prelude.Natural,
    -- | The scaling type. Uniform scaling creates shards of equal size.
    scalingType :: ScalingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateShardCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'updateShardCount_streamName' - The name of the stream.
--
-- 'targetShardCount', 'updateShardCount_targetShardCount' - The new number of shards. This value has the following default limits.
-- By default, you cannot do the following:
--
-- -   Set this value to more than double your current shard count for a
--     stream.
--
-- -   Set this value below half your current shard count for a stream.
--
-- -   Set this value to more than 500 shards in a stream (the default
--     limit for shard count per stream is 500 per account per region),
--     unless you request a limit increase.
--
-- -   Scale a stream with more than 500 shards down unless you set this
--     value to less than 500 shards.
--
-- 'scalingType', 'updateShardCount_scalingType' - The scaling type. Uniform scaling creates shards of equal size.
newUpdateShardCount ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'targetShardCount'
  Prelude.Natural ->
  -- | 'scalingType'
  ScalingType ->
  UpdateShardCount
newUpdateShardCount
  pStreamName_
  pTargetShardCount_
  pScalingType_ =
    UpdateShardCount'
      { streamName = pStreamName_,
        targetShardCount = pTargetShardCount_,
        scalingType = pScalingType_
      }

-- | The name of the stream.
updateShardCount_streamName :: Lens.Lens' UpdateShardCount Prelude.Text
updateShardCount_streamName = Lens.lens (\UpdateShardCount' {streamName} -> streamName) (\s@UpdateShardCount' {} a -> s {streamName = a} :: UpdateShardCount)

-- | The new number of shards. This value has the following default limits.
-- By default, you cannot do the following:
--
-- -   Set this value to more than double your current shard count for a
--     stream.
--
-- -   Set this value below half your current shard count for a stream.
--
-- -   Set this value to more than 500 shards in a stream (the default
--     limit for shard count per stream is 500 per account per region),
--     unless you request a limit increase.
--
-- -   Scale a stream with more than 500 shards down unless you set this
--     value to less than 500 shards.
updateShardCount_targetShardCount :: Lens.Lens' UpdateShardCount Prelude.Natural
updateShardCount_targetShardCount = Lens.lens (\UpdateShardCount' {targetShardCount} -> targetShardCount) (\s@UpdateShardCount' {} a -> s {targetShardCount = a} :: UpdateShardCount)

-- | The scaling type. Uniform scaling creates shards of equal size.
updateShardCount_scalingType :: Lens.Lens' UpdateShardCount ScalingType
updateShardCount_scalingType = Lens.lens (\UpdateShardCount' {scalingType} -> scalingType) (\s@UpdateShardCount' {} a -> s {scalingType = a} :: UpdateShardCount)

instance Core.AWSRequest UpdateShardCount where
  type
    AWSResponse UpdateShardCount =
      UpdateShardCountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateShardCountResponse'
            Prelude.<$> (x Core..?> "TargetShardCount")
            Prelude.<*> (x Core..?> "CurrentShardCount")
            Prelude.<*> (x Core..?> "StreamName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateShardCount

instance Prelude.NFData UpdateShardCount

instance Core.ToHeaders UpdateShardCount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.UpdateShardCount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateShardCount where
  toJSON UpdateShardCount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Core..= streamName),
            Prelude.Just
              ("TargetShardCount" Core..= targetShardCount),
            Prelude.Just ("ScalingType" Core..= scalingType)
          ]
      )

instance Core.ToPath UpdateShardCount where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateShardCount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateShardCountResponse' smart constructor.
data UpdateShardCountResponse = UpdateShardCountResponse'
  { -- | The updated number of shards.
    targetShardCount :: Prelude.Maybe Prelude.Natural,
    -- | The current number of shards.
    currentShardCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the stream.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateShardCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetShardCount', 'updateShardCountResponse_targetShardCount' - The updated number of shards.
--
-- 'currentShardCount', 'updateShardCountResponse_currentShardCount' - The current number of shards.
--
-- 'streamName', 'updateShardCountResponse_streamName' - The name of the stream.
--
-- 'httpStatus', 'updateShardCountResponse_httpStatus' - The response's http status code.
newUpdateShardCountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateShardCountResponse
newUpdateShardCountResponse pHttpStatus_ =
  UpdateShardCountResponse'
    { targetShardCount =
        Prelude.Nothing,
      currentShardCount = Prelude.Nothing,
      streamName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated number of shards.
updateShardCountResponse_targetShardCount :: Lens.Lens' UpdateShardCountResponse (Prelude.Maybe Prelude.Natural)
updateShardCountResponse_targetShardCount = Lens.lens (\UpdateShardCountResponse' {targetShardCount} -> targetShardCount) (\s@UpdateShardCountResponse' {} a -> s {targetShardCount = a} :: UpdateShardCountResponse)

-- | The current number of shards.
updateShardCountResponse_currentShardCount :: Lens.Lens' UpdateShardCountResponse (Prelude.Maybe Prelude.Natural)
updateShardCountResponse_currentShardCount = Lens.lens (\UpdateShardCountResponse' {currentShardCount} -> currentShardCount) (\s@UpdateShardCountResponse' {} a -> s {currentShardCount = a} :: UpdateShardCountResponse)

-- | The name of the stream.
updateShardCountResponse_streamName :: Lens.Lens' UpdateShardCountResponse (Prelude.Maybe Prelude.Text)
updateShardCountResponse_streamName = Lens.lens (\UpdateShardCountResponse' {streamName} -> streamName) (\s@UpdateShardCountResponse' {} a -> s {streamName = a} :: UpdateShardCountResponse)

-- | The response's http status code.
updateShardCountResponse_httpStatus :: Lens.Lens' UpdateShardCountResponse Prelude.Int
updateShardCountResponse_httpStatus = Lens.lens (\UpdateShardCountResponse' {httpStatus} -> httpStatus) (\s@UpdateShardCountResponse' {} a -> s {httpStatus = a} :: UpdateShardCountResponse)

instance Prelude.NFData UpdateShardCountResponse
