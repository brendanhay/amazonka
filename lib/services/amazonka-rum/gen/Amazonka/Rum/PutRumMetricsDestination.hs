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
-- Module      : Amazonka.Rum.PutRumMetricsDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a destination to receive extended metrics from
-- CloudWatch RUM. You can send extended metrics to CloudWatch or to a
-- CloudWatch Evidently experiment.
--
-- For more information about extended metrics, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_AddRumMetrics.html AddRumMetrics>.
module Amazonka.Rum.PutRumMetricsDestination
  ( -- * Creating a Request
    PutRumMetricsDestination (..),
    newPutRumMetricsDestination,

    -- * Request Lenses
    putRumMetricsDestination_destinationArn,
    putRumMetricsDestination_iamRoleArn,
    putRumMetricsDestination_appMonitorName,
    putRumMetricsDestination_destination,

    -- * Destructuring the Response
    PutRumMetricsDestinationResponse (..),
    newPutRumMetricsDestinationResponse,

    -- * Response Lenses
    putRumMetricsDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newPutRumMetricsDestination' smart constructor.
data PutRumMetricsDestination = PutRumMetricsDestination'
  { -- | Use this parameter only if @Destination@ is @Evidently@. This parameter
    -- specifies the ARN of the Evidently experiment that will receive the
    -- extended metrics.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | This parameter is required if @Destination@ is @Evidently@. If
    -- @Destination@ is @CloudWatch@, do not use this parameter.
    --
    -- This parameter specifies the ARN of an IAM role that RUM will assume to
    -- write to the Evidently experiment that you are sending metrics to. This
    -- role must have permission to write to that experiment.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudWatch RUM app monitor that will send the metrics.
    appMonitorName :: Prelude.Text,
    -- | Defines the destination to send the metrics to. Valid values are
    -- @CloudWatch@ and @Evidently@. If you specify @Evidently@, you must also
    -- specify the ARN of the CloudWatchEvidently experiment that is to be the
    -- destination and an IAM role that has permission to write to the
    -- experiment.
    destination :: MetricDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRumMetricsDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'putRumMetricsDestination_destinationArn' - Use this parameter only if @Destination@ is @Evidently@. This parameter
-- specifies the ARN of the Evidently experiment that will receive the
-- extended metrics.
--
-- 'iamRoleArn', 'putRumMetricsDestination_iamRoleArn' - This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of an IAM role that RUM will assume to
-- write to the Evidently experiment that you are sending metrics to. This
-- role must have permission to write to that experiment.
--
-- 'appMonitorName', 'putRumMetricsDestination_appMonitorName' - The name of the CloudWatch RUM app monitor that will send the metrics.
--
-- 'destination', 'putRumMetricsDestination_destination' - Defines the destination to send the metrics to. Valid values are
-- @CloudWatch@ and @Evidently@. If you specify @Evidently@, you must also
-- specify the ARN of the CloudWatchEvidently experiment that is to be the
-- destination and an IAM role that has permission to write to the
-- experiment.
newPutRumMetricsDestination ::
  -- | 'appMonitorName'
  Prelude.Text ->
  -- | 'destination'
  MetricDestination ->
  PutRumMetricsDestination
newPutRumMetricsDestination
  pAppMonitorName_
  pDestination_ =
    PutRumMetricsDestination'
      { destinationArn =
          Prelude.Nothing,
        iamRoleArn = Prelude.Nothing,
        appMonitorName = pAppMonitorName_,
        destination = pDestination_
      }

-- | Use this parameter only if @Destination@ is @Evidently@. This parameter
-- specifies the ARN of the Evidently experiment that will receive the
-- extended metrics.
putRumMetricsDestination_destinationArn :: Lens.Lens' PutRumMetricsDestination (Prelude.Maybe Prelude.Text)
putRumMetricsDestination_destinationArn = Lens.lens (\PutRumMetricsDestination' {destinationArn} -> destinationArn) (\s@PutRumMetricsDestination' {} a -> s {destinationArn = a} :: PutRumMetricsDestination)

-- | This parameter is required if @Destination@ is @Evidently@. If
-- @Destination@ is @CloudWatch@, do not use this parameter.
--
-- This parameter specifies the ARN of an IAM role that RUM will assume to
-- write to the Evidently experiment that you are sending metrics to. This
-- role must have permission to write to that experiment.
putRumMetricsDestination_iamRoleArn :: Lens.Lens' PutRumMetricsDestination (Prelude.Maybe Prelude.Text)
putRumMetricsDestination_iamRoleArn = Lens.lens (\PutRumMetricsDestination' {iamRoleArn} -> iamRoleArn) (\s@PutRumMetricsDestination' {} a -> s {iamRoleArn = a} :: PutRumMetricsDestination)

-- | The name of the CloudWatch RUM app monitor that will send the metrics.
putRumMetricsDestination_appMonitorName :: Lens.Lens' PutRumMetricsDestination Prelude.Text
putRumMetricsDestination_appMonitorName = Lens.lens (\PutRumMetricsDestination' {appMonitorName} -> appMonitorName) (\s@PutRumMetricsDestination' {} a -> s {appMonitorName = a} :: PutRumMetricsDestination)

-- | Defines the destination to send the metrics to. Valid values are
-- @CloudWatch@ and @Evidently@. If you specify @Evidently@, you must also
-- specify the ARN of the CloudWatchEvidently experiment that is to be the
-- destination and an IAM role that has permission to write to the
-- experiment.
putRumMetricsDestination_destination :: Lens.Lens' PutRumMetricsDestination MetricDestination
putRumMetricsDestination_destination = Lens.lens (\PutRumMetricsDestination' {destination} -> destination) (\s@PutRumMetricsDestination' {} a -> s {destination = a} :: PutRumMetricsDestination)

instance Core.AWSRequest PutRumMetricsDestination where
  type
    AWSResponse PutRumMetricsDestination =
      PutRumMetricsDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRumMetricsDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRumMetricsDestination where
  hashWithSalt _salt PutRumMetricsDestination' {..} =
    _salt `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` appMonitorName
      `Prelude.hashWithSalt` destination

instance Prelude.NFData PutRumMetricsDestination where
  rnf PutRumMetricsDestination' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf appMonitorName
      `Prelude.seq` Prelude.rnf destination

instance Data.ToHeaders PutRumMetricsDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRumMetricsDestination where
  toJSON PutRumMetricsDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationArn" Data..=)
              Prelude.<$> destinationArn,
            ("IamRoleArn" Data..=) Prelude.<$> iamRoleArn,
            Prelude.Just ("Destination" Data..= destination)
          ]
      )

instance Data.ToPath PutRumMetricsDestination where
  toPath PutRumMetricsDestination' {..} =
    Prelude.mconcat
      [ "/rummetrics/",
        Data.toBS appMonitorName,
        "/metricsdestination"
      ]

instance Data.ToQuery PutRumMetricsDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRumMetricsDestinationResponse' smart constructor.
data PutRumMetricsDestinationResponse = PutRumMetricsDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRumMetricsDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRumMetricsDestinationResponse_httpStatus' - The response's http status code.
newPutRumMetricsDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRumMetricsDestinationResponse
newPutRumMetricsDestinationResponse pHttpStatus_ =
  PutRumMetricsDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRumMetricsDestinationResponse_httpStatus :: Lens.Lens' PutRumMetricsDestinationResponse Prelude.Int
putRumMetricsDestinationResponse_httpStatus = Lens.lens (\PutRumMetricsDestinationResponse' {httpStatus} -> httpStatus) (\s@PutRumMetricsDestinationResponse' {} a -> s {httpStatus = a} :: PutRumMetricsDestinationResponse)

instance
  Prelude.NFData
    PutRumMetricsDestinationResponse
  where
  rnf PutRumMetricsDestinationResponse' {..} =
    Prelude.rnf httpStatus
