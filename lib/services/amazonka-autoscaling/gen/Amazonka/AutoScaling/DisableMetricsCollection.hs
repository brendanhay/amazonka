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
-- Module      : Amazonka.AutoScaling.DisableMetricsCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables group metrics collection for the specified Auto Scaling group.
module Amazonka.AutoScaling.DisableMetricsCollection
  ( -- * Creating a Request
    DisableMetricsCollection (..),
    newDisableMetricsCollection,

    -- * Request Lenses
    disableMetricsCollection_metrics,
    disableMetricsCollection_autoScalingGroupName,

    -- * Destructuring the Response
    DisableMetricsCollectionResponse (..),
    newDisableMetricsCollectionResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableMetricsCollection' smart constructor.
data DisableMetricsCollection = DisableMetricsCollection'
  { -- | Identifies the metrics to disable.
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
    -- If you omit this property, all metrics are disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableMetricsCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'disableMetricsCollection_metrics' - Identifies the metrics to disable.
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
-- If you omit this property, all metrics are disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'autoScalingGroupName', 'disableMetricsCollection_autoScalingGroupName' - The name of the Auto Scaling group.
newDisableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DisableMetricsCollection
newDisableMetricsCollection pAutoScalingGroupName_ =
  DisableMetricsCollection'
    { metrics =
        Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Identifies the metrics to disable.
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
-- If you omit this property, all metrics are disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-cloudwatch-monitoring.html#as-group-metrics Auto Scaling group metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
disableMetricsCollection_metrics :: Lens.Lens' DisableMetricsCollection (Prelude.Maybe [Prelude.Text])
disableMetricsCollection_metrics = Lens.lens (\DisableMetricsCollection' {metrics} -> metrics) (\s@DisableMetricsCollection' {} a -> s {metrics = a} :: DisableMetricsCollection) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Auto Scaling group.
disableMetricsCollection_autoScalingGroupName :: Lens.Lens' DisableMetricsCollection Prelude.Text
disableMetricsCollection_autoScalingGroupName = Lens.lens (\DisableMetricsCollection' {autoScalingGroupName} -> autoScalingGroupName) (\s@DisableMetricsCollection' {} a -> s {autoScalingGroupName = a} :: DisableMetricsCollection)

instance Core.AWSRequest DisableMetricsCollection where
  type
    AWSResponse DisableMetricsCollection =
      DisableMetricsCollectionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DisableMetricsCollectionResponse'

instance Prelude.Hashable DisableMetricsCollection where
  hashWithSalt _salt DisableMetricsCollection' {..} =
    _salt
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DisableMetricsCollection where
  rnf DisableMetricsCollection' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders DisableMetricsCollection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableMetricsCollection where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableMetricsCollection where
  toQuery DisableMetricsCollection' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisableMetricsCollection" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "Metrics"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> metrics),
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newDisableMetricsCollectionResponse' smart constructor.
data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableMetricsCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableMetricsCollectionResponse ::
  DisableMetricsCollectionResponse
newDisableMetricsCollectionResponse =
  DisableMetricsCollectionResponse'

instance
  Prelude.NFData
    DisableMetricsCollectionResponse
  where
  rnf _ = ()
