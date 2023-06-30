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
-- Module      : Amazonka.EC2.EnableAwsNetworkPerformanceMetricSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Infrastructure Performance subscriptions.
module Amazonka.EC2.EnableAwsNetworkPerformanceMetricSubscription
  ( -- * Creating a Request
    EnableAwsNetworkPerformanceMetricSubscription (..),
    newEnableAwsNetworkPerformanceMetricSubscription,

    -- * Request Lenses
    enableAwsNetworkPerformanceMetricSubscription_destination,
    enableAwsNetworkPerformanceMetricSubscription_dryRun,
    enableAwsNetworkPerformanceMetricSubscription_metric,
    enableAwsNetworkPerformanceMetricSubscription_source,
    enableAwsNetworkPerformanceMetricSubscription_statistic,

    -- * Destructuring the Response
    EnableAwsNetworkPerformanceMetricSubscriptionResponse (..),
    newEnableAwsNetworkPerformanceMetricSubscriptionResponse,

    -- * Response Lenses
    enableAwsNetworkPerformanceMetricSubscriptionResponse_output,
    enableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableAwsNetworkPerformanceMetricSubscription' smart constructor.
data EnableAwsNetworkPerformanceMetricSubscription = EnableAwsNetworkPerformanceMetricSubscription'
  { -- | The target Region or Availability Zone that the metric subscription is
    -- enabled for. For example, @eu-west-1@.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The metric used for the enabled subscription.
    metric :: Prelude.Maybe MetricType,
    -- | The source Region or Availability Zone that the metric subscription is
    -- enabled for. For example, @us-east-1@.
    source :: Prelude.Maybe Prelude.Text,
    -- | The statistic used for the enabled subscription.
    statistic :: Prelude.Maybe StatisticType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAwsNetworkPerformanceMetricSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'enableAwsNetworkPerformanceMetricSubscription_destination' - The target Region or Availability Zone that the metric subscription is
-- enabled for. For example, @eu-west-1@.
--
-- 'dryRun', 'enableAwsNetworkPerformanceMetricSubscription_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'metric', 'enableAwsNetworkPerformanceMetricSubscription_metric' - The metric used for the enabled subscription.
--
-- 'source', 'enableAwsNetworkPerformanceMetricSubscription_source' - The source Region or Availability Zone that the metric subscription is
-- enabled for. For example, @us-east-1@.
--
-- 'statistic', 'enableAwsNetworkPerformanceMetricSubscription_statistic' - The statistic used for the enabled subscription.
newEnableAwsNetworkPerformanceMetricSubscription ::
  EnableAwsNetworkPerformanceMetricSubscription
newEnableAwsNetworkPerformanceMetricSubscription =
  EnableAwsNetworkPerformanceMetricSubscription'
    { destination =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      metric = Prelude.Nothing,
      source = Prelude.Nothing,
      statistic = Prelude.Nothing
    }

-- | The target Region or Availability Zone that the metric subscription is
-- enabled for. For example, @eu-west-1@.
enableAwsNetworkPerformanceMetricSubscription_destination :: Lens.Lens' EnableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe Prelude.Text)
enableAwsNetworkPerformanceMetricSubscription_destination = Lens.lens (\EnableAwsNetworkPerformanceMetricSubscription' {destination} -> destination) (\s@EnableAwsNetworkPerformanceMetricSubscription' {} a -> s {destination = a} :: EnableAwsNetworkPerformanceMetricSubscription)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableAwsNetworkPerformanceMetricSubscription_dryRun :: Lens.Lens' EnableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe Prelude.Bool)
enableAwsNetworkPerformanceMetricSubscription_dryRun = Lens.lens (\EnableAwsNetworkPerformanceMetricSubscription' {dryRun} -> dryRun) (\s@EnableAwsNetworkPerformanceMetricSubscription' {} a -> s {dryRun = a} :: EnableAwsNetworkPerformanceMetricSubscription)

-- | The metric used for the enabled subscription.
enableAwsNetworkPerformanceMetricSubscription_metric :: Lens.Lens' EnableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe MetricType)
enableAwsNetworkPerformanceMetricSubscription_metric = Lens.lens (\EnableAwsNetworkPerformanceMetricSubscription' {metric} -> metric) (\s@EnableAwsNetworkPerformanceMetricSubscription' {} a -> s {metric = a} :: EnableAwsNetworkPerformanceMetricSubscription)

-- | The source Region or Availability Zone that the metric subscription is
-- enabled for. For example, @us-east-1@.
enableAwsNetworkPerformanceMetricSubscription_source :: Lens.Lens' EnableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe Prelude.Text)
enableAwsNetworkPerformanceMetricSubscription_source = Lens.lens (\EnableAwsNetworkPerformanceMetricSubscription' {source} -> source) (\s@EnableAwsNetworkPerformanceMetricSubscription' {} a -> s {source = a} :: EnableAwsNetworkPerformanceMetricSubscription)

-- | The statistic used for the enabled subscription.
enableAwsNetworkPerformanceMetricSubscription_statistic :: Lens.Lens' EnableAwsNetworkPerformanceMetricSubscription (Prelude.Maybe StatisticType)
enableAwsNetworkPerformanceMetricSubscription_statistic = Lens.lens (\EnableAwsNetworkPerformanceMetricSubscription' {statistic} -> statistic) (\s@EnableAwsNetworkPerformanceMetricSubscription' {} a -> s {statistic = a} :: EnableAwsNetworkPerformanceMetricSubscription)

instance
  Core.AWSRequest
    EnableAwsNetworkPerformanceMetricSubscription
  where
  type
    AWSResponse
      EnableAwsNetworkPerformanceMetricSubscription =
      EnableAwsNetworkPerformanceMetricSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableAwsNetworkPerformanceMetricSubscriptionResponse'
            Prelude.<$> (x Data..@? "output")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableAwsNetworkPerformanceMetricSubscription
  where
  hashWithSalt
    _salt
    EnableAwsNetworkPerformanceMetricSubscription' {..} =
      _salt
        `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` metric
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` statistic

instance
  Prelude.NFData
    EnableAwsNetworkPerformanceMetricSubscription
  where
  rnf
    EnableAwsNetworkPerformanceMetricSubscription' {..} =
      Prelude.rnf destination
        `Prelude.seq` Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf metric
        `Prelude.seq` Prelude.rnf source
        `Prelude.seq` Prelude.rnf statistic

instance
  Data.ToHeaders
    EnableAwsNetworkPerformanceMetricSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    EnableAwsNetworkPerformanceMetricSubscription
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    EnableAwsNetworkPerformanceMetricSubscription
  where
  toQuery
    EnableAwsNetworkPerformanceMetricSubscription' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "EnableAwsNetworkPerformanceMetricSubscription" ::
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

-- | /See:/ 'newEnableAwsNetworkPerformanceMetricSubscriptionResponse' smart constructor.
data EnableAwsNetworkPerformanceMetricSubscriptionResponse = EnableAwsNetworkPerformanceMetricSubscriptionResponse'
  { -- | Indicates whether the subscribe action was successful.
    output :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAwsNetworkPerformanceMetricSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'enableAwsNetworkPerformanceMetricSubscriptionResponse_output' - Indicates whether the subscribe action was successful.
--
-- 'httpStatus', 'enableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus' - The response's http status code.
newEnableAwsNetworkPerformanceMetricSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableAwsNetworkPerformanceMetricSubscriptionResponse
newEnableAwsNetworkPerformanceMetricSubscriptionResponse
  pHttpStatus_ =
    EnableAwsNetworkPerformanceMetricSubscriptionResponse'
      { output =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Indicates whether the subscribe action was successful.
enableAwsNetworkPerformanceMetricSubscriptionResponse_output :: Lens.Lens' EnableAwsNetworkPerformanceMetricSubscriptionResponse (Prelude.Maybe Prelude.Bool)
enableAwsNetworkPerformanceMetricSubscriptionResponse_output = Lens.lens (\EnableAwsNetworkPerformanceMetricSubscriptionResponse' {output} -> output) (\s@EnableAwsNetworkPerformanceMetricSubscriptionResponse' {} a -> s {output = a} :: EnableAwsNetworkPerformanceMetricSubscriptionResponse)

-- | The response's http status code.
enableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus :: Lens.Lens' EnableAwsNetworkPerformanceMetricSubscriptionResponse Prelude.Int
enableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus = Lens.lens (\EnableAwsNetworkPerformanceMetricSubscriptionResponse' {httpStatus} -> httpStatus) (\s@EnableAwsNetworkPerformanceMetricSubscriptionResponse' {} a -> s {httpStatus = a} :: EnableAwsNetworkPerformanceMetricSubscriptionResponse)

instance
  Prelude.NFData
    EnableAwsNetworkPerformanceMetricSubscriptionResponse
  where
  rnf
    EnableAwsNetworkPerformanceMetricSubscriptionResponse' {..} =
      Prelude.rnf output
        `Prelude.seq` Prelude.rnf httpStatus
