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

import qualified Network.AWS.GuardDuty.Types.DataSourceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the status of VPC flow logs as a data source.
--
-- /See:/ 'mkFlowLogsConfigurationResult' smart constructor.
newtype FlowLogsConfigurationResult = FlowLogsConfigurationResult'
  { -- | Denotes whether VPC flow logs is enabled as a data source.
    status :: Types.DataSourceStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FlowLogsConfigurationResult' value with any optional fields omitted.
mkFlowLogsConfigurationResult ::
  -- | 'status'
  Types.DataSourceStatus ->
  FlowLogsConfigurationResult
mkFlowLogsConfigurationResult status =
  FlowLogsConfigurationResult' {status}

-- | Denotes whether VPC flow logs is enabled as a data source.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flcrStatus :: Lens.Lens' FlowLogsConfigurationResult Types.DataSourceStatus
flcrStatus = Lens.field @"status"
{-# DEPRECATED flcrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON FlowLogsConfigurationResult where
  parseJSON =
    Core.withObject "FlowLogsConfigurationResult" Core.$
      \x -> FlowLogsConfigurationResult' Core.<$> (x Core..: "status")
