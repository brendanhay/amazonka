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
-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables group metrics for the specified Auto Scaling group. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-monitoring.html Monitoring CloudWatch metrics for your Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.EnableMetricsCollection
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

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableMetricsCollection' smart constructor.
data EnableMetricsCollection = EnableMetricsCollection'
  { -- | Specifies which group-level metrics to start collecting. You can specify
    -- one or more of the following metrics:
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
    -- The instance weighting feature supports the following additional
    -- metrics:
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
    -- If you omit this parameter, all metrics are enabled.
    metrics :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The granularity to associate with the metrics to collect. The only valid
    -- value is @1Minute@.
    granularity :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableMetricsCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'enableMetricsCollection_metrics' - Specifies which group-level metrics to start collecting. You can specify
-- one or more of the following metrics:
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
-- The instance weighting feature supports the following additional
-- metrics:
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
-- If you omit this parameter, all metrics are enabled.
--
-- 'autoScalingGroupName', 'enableMetricsCollection_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'granularity', 'enableMetricsCollection_granularity' - The granularity to associate with the metrics to collect. The only valid
-- value is @1Minute@.
newEnableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  -- | 'granularity'
  Core.Text ->
  EnableMetricsCollection
newEnableMetricsCollection
  pAutoScalingGroupName_
  pGranularity_ =
    EnableMetricsCollection'
      { metrics = Core.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        granularity = pGranularity_
      }

-- | Specifies which group-level metrics to start collecting. You can specify
-- one or more of the following metrics:
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
-- The instance weighting feature supports the following additional
-- metrics:
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
-- If you omit this parameter, all metrics are enabled.
enableMetricsCollection_metrics :: Lens.Lens' EnableMetricsCollection (Core.Maybe [Core.Text])
enableMetricsCollection_metrics = Lens.lens (\EnableMetricsCollection' {metrics} -> metrics) (\s@EnableMetricsCollection' {} a -> s {metrics = a} :: EnableMetricsCollection) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
enableMetricsCollection_autoScalingGroupName :: Lens.Lens' EnableMetricsCollection Core.Text
enableMetricsCollection_autoScalingGroupName = Lens.lens (\EnableMetricsCollection' {autoScalingGroupName} -> autoScalingGroupName) (\s@EnableMetricsCollection' {} a -> s {autoScalingGroupName = a} :: EnableMetricsCollection)

-- | The granularity to associate with the metrics to collect. The only valid
-- value is @1Minute@.
enableMetricsCollection_granularity :: Lens.Lens' EnableMetricsCollection Core.Text
enableMetricsCollection_granularity = Lens.lens (\EnableMetricsCollection' {granularity} -> granularity) (\s@EnableMetricsCollection' {} a -> s {granularity = a} :: EnableMetricsCollection)

instance Core.AWSRequest EnableMetricsCollection where
  type
    AWSResponse EnableMetricsCollection =
      EnableMetricsCollectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      EnableMetricsCollectionResponse'

instance Core.Hashable EnableMetricsCollection

instance Core.NFData EnableMetricsCollection

instance Core.ToHeaders EnableMetricsCollection where
  toHeaders = Core.const Core.mempty

instance Core.ToPath EnableMetricsCollection where
  toPath = Core.const "/"

instance Core.ToQuery EnableMetricsCollection where
  toQuery EnableMetricsCollection' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("EnableMetricsCollection" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "Metrics"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> metrics),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "Granularity" Core.=: granularity
      ]

-- | /See:/ 'newEnableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableMetricsCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableMetricsCollectionResponse ::
  EnableMetricsCollectionResponse
newEnableMetricsCollectionResponse =
  EnableMetricsCollectionResponse'

instance Core.NFData EnableMetricsCollectionResponse
