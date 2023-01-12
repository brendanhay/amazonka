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
-- Module      : Amazonka.BackupGateway.PutBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action sets the bandwidth rate limit schedule for a specified
-- gateway. By default, gateways do not have a bandwidth rate limit
-- schedule, which means no bandwidth rate limiting is in effect. Use this
-- to initiate a gateway\'s bandwidth rate limit schedule.
module Amazonka.BackupGateway.PutBandwidthRateLimitSchedule
  ( -- * Creating a Request
    PutBandwidthRateLimitSchedule (..),
    newPutBandwidthRateLimitSchedule,

    -- * Request Lenses
    putBandwidthRateLimitSchedule_bandwidthRateLimitIntervals,
    putBandwidthRateLimitSchedule_gatewayArn,

    -- * Destructuring the Response
    PutBandwidthRateLimitScheduleResponse (..),
    newPutBandwidthRateLimitScheduleResponse,

    -- * Response Lenses
    putBandwidthRateLimitScheduleResponse_gatewayArn,
    putBandwidthRateLimitScheduleResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutBandwidthRateLimitSchedule' smart constructor.
data PutBandwidthRateLimitSchedule = PutBandwidthRateLimitSchedule'
  { -- | An array containing bandwidth rate limit schedule intervals for a
    -- gateway. When no bandwidth rate limit intervals have been scheduled, the
    -- array is empty.
    bandwidthRateLimitIntervals :: [BandwidthRateLimitInterval],
    -- | The Amazon Resource Name (ARN) of the gateway. Use the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBandwidthRateLimitSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthRateLimitIntervals', 'putBandwidthRateLimitSchedule_bandwidthRateLimitIntervals' - An array containing bandwidth rate limit schedule intervals for a
-- gateway. When no bandwidth rate limit intervals have been scheduled, the
-- array is empty.
--
-- 'gatewayArn', 'putBandwidthRateLimitSchedule_gatewayArn' - The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
newPutBandwidthRateLimitSchedule ::
  -- | 'gatewayArn'
  Prelude.Text ->
  PutBandwidthRateLimitSchedule
newPutBandwidthRateLimitSchedule pGatewayArn_ =
  PutBandwidthRateLimitSchedule'
    { bandwidthRateLimitIntervals =
        Prelude.mempty,
      gatewayArn = pGatewayArn_
    }

-- | An array containing bandwidth rate limit schedule intervals for a
-- gateway. When no bandwidth rate limit intervals have been scheduled, the
-- array is empty.
putBandwidthRateLimitSchedule_bandwidthRateLimitIntervals :: Lens.Lens' PutBandwidthRateLimitSchedule [BandwidthRateLimitInterval]
putBandwidthRateLimitSchedule_bandwidthRateLimitIntervals = Lens.lens (\PutBandwidthRateLimitSchedule' {bandwidthRateLimitIntervals} -> bandwidthRateLimitIntervals) (\s@PutBandwidthRateLimitSchedule' {} a -> s {bandwidthRateLimitIntervals = a} :: PutBandwidthRateLimitSchedule) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
putBandwidthRateLimitSchedule_gatewayArn :: Lens.Lens' PutBandwidthRateLimitSchedule Prelude.Text
putBandwidthRateLimitSchedule_gatewayArn = Lens.lens (\PutBandwidthRateLimitSchedule' {gatewayArn} -> gatewayArn) (\s@PutBandwidthRateLimitSchedule' {} a -> s {gatewayArn = a} :: PutBandwidthRateLimitSchedule)

instance
  Core.AWSRequest
    PutBandwidthRateLimitSchedule
  where
  type
    AWSResponse PutBandwidthRateLimitSchedule =
      PutBandwidthRateLimitScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutBandwidthRateLimitScheduleResponse'
            Prelude.<$> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutBandwidthRateLimitSchedule
  where
  hashWithSalt _salt PutBandwidthRateLimitSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` bandwidthRateLimitIntervals
      `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData PutBandwidthRateLimitSchedule where
  rnf PutBandwidthRateLimitSchedule' {..} =
    Prelude.rnf bandwidthRateLimitIntervals
      `Prelude.seq` Prelude.rnf gatewayArn

instance Data.ToHeaders PutBandwidthRateLimitSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.PutBandwidthRateLimitSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutBandwidthRateLimitSchedule where
  toJSON PutBandwidthRateLimitSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "BandwidthRateLimitIntervals"
                  Data..= bandwidthRateLimitIntervals
              ),
            Prelude.Just ("GatewayArn" Data..= gatewayArn)
          ]
      )

instance Data.ToPath PutBandwidthRateLimitSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery PutBandwidthRateLimitSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutBandwidthRateLimitScheduleResponse' smart constructor.
data PutBandwidthRateLimitScheduleResponse = PutBandwidthRateLimitScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBandwidthRateLimitScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'putBandwidthRateLimitScheduleResponse_gatewayArn' - The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
--
-- 'httpStatus', 'putBandwidthRateLimitScheduleResponse_httpStatus' - The response's http status code.
newPutBandwidthRateLimitScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutBandwidthRateLimitScheduleResponse
newPutBandwidthRateLimitScheduleResponse pHttpStatus_ =
  PutBandwidthRateLimitScheduleResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
putBandwidthRateLimitScheduleResponse_gatewayArn :: Lens.Lens' PutBandwidthRateLimitScheduleResponse (Prelude.Maybe Prelude.Text)
putBandwidthRateLimitScheduleResponse_gatewayArn = Lens.lens (\PutBandwidthRateLimitScheduleResponse' {gatewayArn} -> gatewayArn) (\s@PutBandwidthRateLimitScheduleResponse' {} a -> s {gatewayArn = a} :: PutBandwidthRateLimitScheduleResponse)

-- | The response's http status code.
putBandwidthRateLimitScheduleResponse_httpStatus :: Lens.Lens' PutBandwidthRateLimitScheduleResponse Prelude.Int
putBandwidthRateLimitScheduleResponse_httpStatus = Lens.lens (\PutBandwidthRateLimitScheduleResponse' {httpStatus} -> httpStatus) (\s@PutBandwidthRateLimitScheduleResponse' {} a -> s {httpStatus = a} :: PutBandwidthRateLimitScheduleResponse)

instance
  Prelude.NFData
    PutBandwidthRateLimitScheduleResponse
  where
  rnf PutBandwidthRateLimitScheduleResponse' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf httpStatus
