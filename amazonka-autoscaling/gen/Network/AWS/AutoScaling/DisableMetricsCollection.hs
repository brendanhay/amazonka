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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DisableMetricsCollection
newDisableMetricsCollection pAutoScalingGroupName_ =
  DisableMetricsCollection'
    { metrics =
        Prelude.Nothing,
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
disableMetricsCollection_metrics :: Lens.Lens' DisableMetricsCollection (Prelude.Maybe [Prelude.Text])
disableMetricsCollection_metrics = Lens.lens (\DisableMetricsCollection' {metrics} -> metrics) (\s@DisableMetricsCollection' {} a -> s {metrics = a} :: DisableMetricsCollection) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Auto Scaling group.
disableMetricsCollection_autoScalingGroupName :: Lens.Lens' DisableMetricsCollection Prelude.Text
disableMetricsCollection_autoScalingGroupName = Lens.lens (\DisableMetricsCollection' {autoScalingGroupName} -> autoScalingGroupName) (\s@DisableMetricsCollection' {} a -> s {autoScalingGroupName = a} :: DisableMetricsCollection)

instance Prelude.AWSRequest DisableMetricsCollection where
  type
    Rs DisableMetricsCollection =
      DisableMetricsCollectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DisableMetricsCollectionResponse'

instance Prelude.Hashable DisableMetricsCollection

instance Prelude.NFData DisableMetricsCollection

instance Prelude.ToHeaders DisableMetricsCollection where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DisableMetricsCollection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableMetricsCollection where
  toQuery DisableMetricsCollection' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DisableMetricsCollection" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "Metrics"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> metrics),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName
      ]

-- | /See:/ 'newDisableMetricsCollectionResponse' smart constructor.
data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
