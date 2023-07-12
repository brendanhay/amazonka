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
-- Module      : Amazonka.EC2.DisableAwsNetworkPerformanceMetricSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables Infrastructure Performance metric subscriptions.
module Amazonka.EC2.DisableAwsNetworkPerformanceMetricSubscription
  ( -- * Creating a Request
    DisableAwsNetworkPerformanceMetricSubscription (..),
    newDisableAwsNetworkPerformanceMetricSubscription,

    -- * Request Lenses
    disableAwsNetworkPerformanceMetricSubscription_destination,
    disableAwsNetworkPerformanceMetricSubscription_dryRun,
    disableAwsNetworkPerformanceMetricSubscription_metric,
    disableAwsNetworkPerformanceMetricSubscription_source,
    disableAwsNetworkPerformanceMetricSubscription_statistic,

    -- * Destructuring the Response
    DisableAwsNetworkPerformanceMetricSubscriptionResponse (..),
    newDisableAwsNetworkPerformanceMetricSubscriptionResponse,

    -- * Response Lenses
    disableAwsNetworkPerformanceMetricSubscriptionResponse_output,
    disableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableAwsNetworkPerformanceMetricSubscription' smart constructor.
data DisableAwsNetworkPerformanceMetricSubscription = DisableAwsNetworkPerformanceMetricSubscription'
  { -- | The target Region or Availability Zone that the metric subscription is
    -- disabled for. For example, @eu-north-1@.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The metric used for the disabled subscription.
    metric :: Prelude.Maybe MetricType,
    -- | The source Region or Availability Zone that the metric subscription is
    -- disabled for. For example, @us-east-1@.
    source :: Prelude.Maybe Prelude.Text,
    -- | The statistic used for the disabled subscription.
    statistic :: Prelude.Maybe StatisticType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAwsNetworkPerformanceMetricSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'disableAwsNetworkPerformanceMetricSubscription_destination' - The target Region or Availability Zone that the metric subscription is
-- disabled for. For example, @eu-north-1@.
--
-- 'dryRun', 'disableAwsNetworkPerformanceMetricSubscription_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'metric', 'disableAwsNetworkPerformanceMetricSubscription_metric' - The metric used for the disabled subscription.
--
-- 'source', 'disableAwsNetworkPerformanceMetricSubscription_source' - The source Region or Availability Zone that the metric subscription is
-- disabled for. For example, @us-east-1@.
--
-- 'statistic', 'disableAwsNetworkPerformanceMetricSubscription_statistic' - The statistic used for the disabled subscription.
newDisableAwsNetworkPerformanceMetricSubscription ::
  DisableAwsNetworkPerformanceMetricSubscription
newDisableAwsNetworkPerformanceMetricSubscription =
  DisableAwsNetworkPerformanceMetricSubscription'
    { destination =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      metric = Prelude.Nothing,
      source = Prelude.Nothing,
      statistic = Prelude.Nothing
    }

-- | The target Region or Availability Zone that the metric subscription is
-- disabled for. For example, @eu-north-1@.
disableAwsNetworkPerformanceMetricSubscription_destination :: Lens.Lens' DisableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe Prelude.Text)
disableAwsNetworkPerformanceMetricSubscription_destination = Lens.lens (\DisableAwsNetworkPerformanceMetricSubscription' {destination} -> destination) (\s@DisableAwsNetworkPerformanceMetricSubscription' {} a -> s {destination = a} :: DisableAwsNetworkPerformanceMetricSubscription)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableAwsNetworkPerformanceMetricSubscription_dryRun :: Lens.Lens' DisableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe Prelude.Bool)
disableAwsNetworkPerformanceMetricSubscription_dryRun = Lens.lens (\DisableAwsNetworkPerformanceMetricSubscription' {dryRun} -> dryRun) (\s@DisableAwsNetworkPerformanceMetricSubscription' {} a -> s {dryRun = a} :: DisableAwsNetworkPerformanceMetricSubscription)

-- | The metric used for the disabled subscription.
disableAwsNetworkPerformanceMetricSubscription_metric :: Lens.Lens' DisableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe MetricType)
disableAwsNetworkPerformanceMetricSubscription_metric = Lens.lens (\DisableAwsNetworkPerformanceMetricSubscription' {metric} -> metric) (\s@DisableAwsNetworkPerformanceMetricSubscription' {} a -> s {metric = a} :: DisableAwsNetworkPerformanceMetricSubscription)

-- | The source Region or Availability Zone that the metric subscription is
-- disabled for. For example, @us-east-1@.
disableAwsNetworkPerformanceMetricSubscription_source :: Lens.Lens' DisableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe Prelude.Text)
disableAwsNetworkPerformanceMetricSubscription_source = Lens.lens (\DisableAwsNetworkPerformanceMetricSubscription' {source} -> source) (\s@DisableAwsNetworkPerformanceMetricSubscription' {} a -> s {source = a} :: DisableAwsNetworkPerformanceMetricSubscription)

-- | The statistic used for the disabled subscription.
disableAwsNetworkPerformanceMetricSubscription_statistic :: Lens.Lens' DisableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe StatisticType)
disableAwsNetworkPerformanceMetricSubscription_statistic = Lens.lens (\DisableAwsNetworkPerformanceMetricSubscription' {statistic} -> statistic) (\s@DisableAwsNetworkPerformanceMetricSubscription' {} a -> s {statistic = a} :: DisableAwsNetworkPerformanceMetricSubscription)

instance
  Core.AWSRequest
    DisableAwsNetworkPerformanceMetricSubscription
  where
  type
    AWSResponse
      DisableAwsNetworkPerformanceMetricSubscription =
      DisableAwsNetworkPerformanceMetricSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableAwsNetworkPerformanceMetricSubscriptionResponse'
            Prelude.<$> (x Data..@? "output")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableAwsNetworkPerformanceMetricSubscription
  where
  hashWithSalt
    _salt
    DisableAwsNetworkPerformanceMetricSubscription' {..} =
      _salt
        `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` metric
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` statistic

instance
  Prelude.NFData
    DisableAwsNetworkPerformanceMetricSubscription
  where
  rnf
    DisableAwsNetworkPerformanceMetricSubscription' {..} =
      Prelude.rnf destination
        `Prelude.seq` Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf metric
        `Prelude.seq` Prelude.rnf source
        `Prelude.seq` Prelude.rnf statistic

instance
  Data.ToHeaders
    DisableAwsNetworkPerformanceMetricSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisableAwsNetworkPerformanceMetricSubscription
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisableAwsNetworkPerformanceMetricSubscription
  where
  toQuery
    DisableAwsNetworkPerformanceMetricSubscription' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DisableAwsNetworkPerformanceMetricSubscription" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "Destination" Data.=: destination,
          "DryRun" Data.=: dryRun,
          "Metric" Data.=: metric,
          "Source" Data.=: source,
          "Statistic" Data.=: statistic
        ]

-- | /See:/ 'newDisableAwsNetworkPerformanceMetricSubscriptionResponse' smart constructor.
data DisableAwsNetworkPerformanceMetricSubscriptionResponse = DisableAwsNetworkPerformanceMetricSubscriptionResponse'
  { -- | Indicates whether the unsubscribe action was successful.
    output :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAwsNetworkPerformanceMetricSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'disableAwsNetworkPerformanceMetricSubscriptionResponse_output' - Indicates whether the unsubscribe action was successful.
--
-- 'httpStatus', 'disableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus' - The response's http status code.
newDisableAwsNetworkPerformanceMetricSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableAwsNetworkPerformanceMetricSubscriptionResponse
newDisableAwsNetworkPerformanceMetricSubscriptionResponse
  pHttpStatus_ =
    DisableAwsNetworkPerformanceMetricSubscriptionResponse'
      { output =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Indicates whether the unsubscribe action was successful.
disableAwsNetworkPerformanceMetricSubscriptionResponse_output :: Lens.Lens' DisableAwsNetworkPerformanceMetricSubscriptionResponse (Prelude.Maybe Prelude.Bool)
disableAwsNetworkPerformanceMetricSubscriptionResponse_output = Lens.lens (\DisableAwsNetworkPerformanceMetricSubscriptionResponse' {output} -> output) (\s@DisableAwsNetworkPerformanceMetricSubscriptionResponse' {} a -> s {output = a} :: DisableAwsNetworkPerformanceMetricSubscriptionResponse)

-- | The response's http status code.
disableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus :: Lens.Lens' DisableAwsNetworkPerformanceMetricSubscriptionResponse Prelude.Int
disableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus = Lens.lens (\DisableAwsNetworkPerformanceMetricSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DisableAwsNetworkPerformanceMetricSubscriptionResponse' {} a -> s {httpStatus = a} :: DisableAwsNetworkPerformanceMetricSubscriptionResponse)

instance
  Prelude.NFData
    DisableAwsNetworkPerformanceMetricSubscriptionResponse
  where
  rnf
    DisableAwsNetworkPerformanceMetricSubscriptionResponse' {..} =
      Prelude.rnf output
        `Prelude.seq` Prelude.rnf httpStatus
