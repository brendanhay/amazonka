{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
  ( FlowLogsConfigurationResult (..),

    -- * Smart constructor
    mkFlowLogsConfigurationResult,

    -- * Lenses
    flcrStatus,
  )
where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the status of VPC flow logs as a data source.
--
-- /See:/ 'mkFlowLogsConfigurationResult' smart constructor.
newtype FlowLogsConfigurationResult = FlowLogsConfigurationResult'
  { -- | Denotes whether VPC flow logs is enabled as a data source.
    status :: DataSourceStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlowLogsConfigurationResult' with the minimum fields required to make a request.
--
-- * 'status' - Denotes whether VPC flow logs is enabled as a data source.
mkFlowLogsConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  FlowLogsConfigurationResult
mkFlowLogsConfigurationResult pStatus_ =
  FlowLogsConfigurationResult' {status = pStatus_}

-- | Denotes whether VPC flow logs is enabled as a data source.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flcrStatus :: Lens.Lens' FlowLogsConfigurationResult DataSourceStatus
flcrStatus = Lens.lens (status :: FlowLogsConfigurationResult -> DataSourceStatus) (\s a -> s {status = a} :: FlowLogsConfigurationResult)
{-# DEPRECATED flcrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON FlowLogsConfigurationResult where
  parseJSON =
    Lude.withObject
      "FlowLogsConfigurationResult"
      (\x -> FlowLogsConfigurationResult' Lude.<$> (x Lude..: "status"))
