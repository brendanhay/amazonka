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
-- Module      : Network.AWS.AutoScaling.DisableMetricsCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables group metrics for the specified Auto Scaling group.
module Network.AWS.AutoScaling.DisableMetricsCollection
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

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableMetricsCollection' smart constructor.
data DisableMetricsCollection = DisableMetricsCollection'
  { -- | Specifies one or more of the following metrics:
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
    -- If you omit this parameter, all metrics are disabled.
    metrics :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableMetricsCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'disableMetricsCollection_metrics' - Specifies one or more of the following metrics:
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
-- If you omit this parameter, all metrics are disabled.
--
-- 'autoScalingGroupName', 'disableMetricsCollection_autoScalingGroupName' - The name of the Auto Scaling group.
newDisableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  DisableMetricsCollection
newDisableMetricsCollection pAutoScalingGroupName_ =
  DisableMetricsCollection'
    { metrics = Core.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Specifies one or more of the following metrics:
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
-- If you omit this parameter, all metrics are disabled.
disableMetricsCollection_metrics :: Lens.Lens' DisableMetricsCollection (Core.Maybe [Core.Text])
disableMetricsCollection_metrics = Lens.lens (\DisableMetricsCollection' {metrics} -> metrics) (\s@DisableMetricsCollection' {} a -> s {metrics = a} :: DisableMetricsCollection) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
disableMetricsCollection_autoScalingGroupName :: Lens.Lens' DisableMetricsCollection Core.Text
disableMetricsCollection_autoScalingGroupName = Lens.lens (\DisableMetricsCollection' {autoScalingGroupName} -> autoScalingGroupName) (\s@DisableMetricsCollection' {} a -> s {autoScalingGroupName = a} :: DisableMetricsCollection)

instance Core.AWSRequest DisableMetricsCollection where
  type
    AWSResponse DisableMetricsCollection =
      DisableMetricsCollectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DisableMetricsCollectionResponse'

instance Core.Hashable DisableMetricsCollection

instance Core.NFData DisableMetricsCollection

instance Core.ToHeaders DisableMetricsCollection where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DisableMetricsCollection where
  toPath = Core.const "/"

instance Core.ToQuery DisableMetricsCollection where
  toQuery DisableMetricsCollection' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DisableMetricsCollection" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "Metrics"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> metrics),
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDisableMetricsCollectionResponse' smart constructor.
data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableMetricsCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableMetricsCollectionResponse ::
  DisableMetricsCollectionResponse
newDisableMetricsCollectionResponse =
  DisableMetricsCollectionResponse'

instance Core.NFData DisableMetricsCollectionResponse
