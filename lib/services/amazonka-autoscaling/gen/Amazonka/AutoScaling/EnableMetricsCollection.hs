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
-- Module      : Amazonka.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables group metrics collection for the specified Auto Scaling group.
--
-- You can use these metrics to track changes in an Auto Scaling group and
-- to set alarms on threshold values. You can view group metrics using the
-- Amazon EC2 Auto Scaling console or the CloudWatch console. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html Monitor CloudWatch metrics for your Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.EnableMetricsCollection
  ( -- * Creating a Request
    EnableMetricsCollection (..),
    newEnableMetricsCollection,

    -- * Request Lenses
    enableMetricsCollection_metrics,
    enableMetricsCollection_autoScalingGroupName,
    enableMetricsCollection_granularity,

    -- * Destructuring the Response
    EnableMetricsCollectionResponse (..),
    newEnableMetricsCollectionResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableMetricsCollection' smart constructor.
data EnableMetricsCollection = EnableMetricsCollection'
  { -- | Identifies the metrics to enable.
    --
    -- You can specify one or more of the following metrics:
    --
    -- -   @GroupMinSize@
    --
    -- -   @GroupMaxSize@
    --
    -- -   @GroupDesiredCapacity@
    --
    -- -   @GroupInServiceInstances@
    --
    -- -   @GroupPendingInstances@
    --
    -- -   @GroupStandbyInstances@
    --
    -- -   @GroupTerminatingInstances@
    --
    -- -   @GroupTotalInstances@
    --
    -- -   @GroupInServiceCapacity@
    --
    -- -   @GroupPendingCapacity@
    --
    -- -   @GroupStandbyCapacity@
    --
    -- -   @GroupTerminatingCapacity@
    --
    -- -   @GroupTotalCapacity@
    --
    -- -   @WarmPoolDesiredCapacity@
    --
    -- -   @WarmPoolWarmedCapacity@
    --
    -- -   @WarmPoolPendingCapacity@
    --
    -- -   @WarmPoolTerminatingCapacity@
    --
    -- -   @WarmPoolTotalCapacity@
    --
    -- -   @GroupAndWarmPoolDesiredCapacity@
    --
    -- -   @GroupAndWarmPoolTotalCapacity@
    --
    -- If you specify @Granularity@ and don\'t specify any metrics, all metrics
    -- are enabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The frequency at which Amazon EC2 Auto Scaling sends aggregated data to
    -- CloudWatch. The only valid value is @1Minute@.
    granularity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableMetricsCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'enableMetricsCollection_metrics' - Identifies the metrics to enable.
--
-- You can specify one or more of the following metrics:
--
-- -   @GroupMinSize@
--
-- -   @GroupMaxSize@
--
-- -   @GroupDesiredCapacity@
--
-- -   @GroupInServiceInstances@
--
-- -   @GroupPendingInstances@
--
-- -   @GroupStandbyInstances@
--
-- -   @GroupTerminatingInstances@
--
-- -   @GroupTotalInstances@
--
-- -   @GroupInServiceCapacity@
--
-- -   @GroupPendingCapacity@
--
-- -   @GroupStandbyCapacity@
--
-- -   @GroupTerminatingCapacity@
--
-- -   @GroupTotalCapacity@
--
-- -   @WarmPoolDesiredCapacity@
--
-- -   @WarmPoolWarmedCapacity@
--
-- -   @WarmPoolPendingCapacity@
--
-- -   @WarmPoolTerminatingCapacity@
--
-- -   @WarmPoolTotalCapacity@
--
-- -   @GroupAndWarmPoolDesiredCapacity@
--
-- -   @GroupAndWarmPoolTotalCapacity@
--
-- If you specify @Granularity@ and don\'t specify any metrics, all metrics
-- are enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'autoScalingGroupName', 'enableMetricsCollection_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'granularity', 'enableMetricsCollection_granularity' - The frequency at which Amazon EC2 Auto Scaling sends aggregated data to
-- CloudWatch. The only valid value is @1Minute@.
newEnableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'granularity'
  Prelude.Text ->
  EnableMetricsCollection
newEnableMetricsCollection
  pAutoScalingGroupName_
  pGranularity_ =
    EnableMetricsCollection'
      { metrics = Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        granularity = pGranularity_
      }

-- | Identifies the metrics to enable.
--
-- You can specify one or more of the following metrics:
--
-- -   @GroupMinSize@
--
-- -   @GroupMaxSize@
--
-- -   @GroupDesiredCapacity@
--
-- -   @GroupInServiceInstances@
--
-- -   @GroupPendingInstances@
--
-- -   @GroupStandbyInstances@
--
-- -   @GroupTerminatingInstances@
--
-- -   @GroupTotalInstances@
--
-- -   @GroupInServiceCapacity@
--
-- -   @GroupPendingCapacity@
--
-- -   @GroupStandbyCapacity@
--
-- -   @GroupTerminatingCapacity@
--
-- -   @GroupTotalCapacity@
--
-- -   @WarmPoolDesiredCapacity@
--
-- -   @WarmPoolWarmedCapacity@
--
-- -   @WarmPoolPendingCapacity@
--
-- -   @WarmPoolTerminatingCapacity@
--
-- -   @WarmPoolTotalCapacity@
--
-- -   @GroupAndWarmPoolDesiredCapacity@
--
-- -   @GroupAndWarmPoolTotalCapacity@
--
-- If you specify @Granularity@ and don\'t specify any metrics, all metrics
-- are enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
enableMetricsCollection_metrics :: Lens.Lens' EnableMetricsCollection (Prelude.Maybe [Prelude.Text])
enableMetricsCollection_metrics = Lens.lens (\EnableMetricsCollection' {metrics} -> metrics) (\s@EnableMetricsCollection' {} a -> s {metrics = a} :: EnableMetricsCollection) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Auto Scaling group.
enableMetricsCollection_autoScalingGroupName :: Lens.Lens' EnableMetricsCollection Prelude.Text
enableMetricsCollection_autoScalingGroupName = Lens.lens (\EnableMetricsCollection' {autoScalingGroupName} -> autoScalingGroupName) (\s@EnableMetricsCollection' {} a -> s {autoScalingGroupName = a} :: EnableMetricsCollection)

-- | The frequency at which Amazon EC2 Auto Scaling sends aggregated data to
-- CloudWatch. The only valid value is @1Minute@.
enableMetricsCollection_granularity :: Lens.Lens' EnableMetricsCollection Prelude.Text
enableMetricsCollection_granularity = Lens.lens (\EnableMetricsCollection' {granularity} -> granularity) (\s@EnableMetricsCollection' {} a -> s {granularity = a} :: EnableMetricsCollection)

instance Core.AWSRequest EnableMetricsCollection where
  type
    AWSResponse EnableMetricsCollection =
      EnableMetricsCollectionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      EnableMetricsCollectionResponse'

instance Prelude.Hashable EnableMetricsCollection where
  hashWithSalt _salt EnableMetricsCollection' {..} =
    _salt
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` granularity

instance Prelude.NFData EnableMetricsCollection where
  rnf EnableMetricsCollection' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf granularity

instance Data.ToHeaders EnableMetricsCollection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableMetricsCollection where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableMetricsCollection where
  toQuery EnableMetricsCollection' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableMetricsCollection" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "Metrics"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> metrics),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "Granularity" Data.=: granularity
      ]

-- | /See:/ 'newEnableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableMetricsCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableMetricsCollectionResponse ::
  EnableMetricsCollectionResponse
newEnableMetricsCollectionResponse =
  EnableMetricsCollectionResponse'

instance
  Prelude.NFData
    EnableMetricsCollectionResponse
  where
  rnf _ = ()
