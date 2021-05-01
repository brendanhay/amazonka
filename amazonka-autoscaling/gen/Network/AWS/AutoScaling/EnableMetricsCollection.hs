{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The granularity to associate with the metrics to collect. The only valid
    -- value is @1Minute@.
    granularity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
enableMetricsCollection_metrics :: Lens.Lens' EnableMetricsCollection (Prelude.Maybe [Prelude.Text])
enableMetricsCollection_metrics = Lens.lens (\EnableMetricsCollection' {metrics} -> metrics) (\s@EnableMetricsCollection' {} a -> s {metrics = a} :: EnableMetricsCollection) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Auto Scaling group.
enableMetricsCollection_autoScalingGroupName :: Lens.Lens' EnableMetricsCollection Prelude.Text
enableMetricsCollection_autoScalingGroupName = Lens.lens (\EnableMetricsCollection' {autoScalingGroupName} -> autoScalingGroupName) (\s@EnableMetricsCollection' {} a -> s {autoScalingGroupName = a} :: EnableMetricsCollection)

-- | The granularity to associate with the metrics to collect. The only valid
-- value is @1Minute@.
enableMetricsCollection_granularity :: Lens.Lens' EnableMetricsCollection Prelude.Text
enableMetricsCollection_granularity = Lens.lens (\EnableMetricsCollection' {granularity} -> granularity) (\s@EnableMetricsCollection' {} a -> s {granularity = a} :: EnableMetricsCollection)

instance Prelude.AWSRequest EnableMetricsCollection where
  type
    Rs EnableMetricsCollection =
      EnableMetricsCollectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      EnableMetricsCollectionResponse'

instance Prelude.Hashable EnableMetricsCollection

instance Prelude.NFData EnableMetricsCollection

instance Prelude.ToHeaders EnableMetricsCollection where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath EnableMetricsCollection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableMetricsCollection where
  toQuery EnableMetricsCollection' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("EnableMetricsCollection" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "Metrics"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> metrics),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName,
        "Granularity" Prelude.=: granularity
      ]

-- | /See:/ 'newEnableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
