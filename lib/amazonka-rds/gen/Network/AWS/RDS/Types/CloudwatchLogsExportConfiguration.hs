{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
  ( CloudwatchLogsExportConfiguration (..),

    -- * Smart constructor
    mkCloudwatchLogsExportConfiguration,

    -- * Lenses
    clecDisableLogTypes,
    clecEnableLogTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance or DB cluster.
--
-- The @EnableLogTypes@ and @DisableLogTypes@ arrays determine which logs will be exported (or not exported) to CloudWatch Logs. The values within these arrays depend on the DB engine being used.
-- For more information about exporting CloudWatch Logs for Amazon RDS DB instances, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
-- For more information about exporting CloudWatch Logs for Amazon Aurora DB clusters, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- /See:/ 'mkCloudwatchLogsExportConfiguration' smart constructor.
data CloudwatchLogsExportConfiguration = CloudwatchLogsExportConfiguration'
  { -- | The list of log types to disable.
    disableLogTypes :: Core.Maybe [Types.String],
    -- | The list of log types to enable.
    enableLogTypes :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudwatchLogsExportConfiguration' value with any optional fields omitted.
mkCloudwatchLogsExportConfiguration ::
  CloudwatchLogsExportConfiguration
mkCloudwatchLogsExportConfiguration =
  CloudwatchLogsExportConfiguration'
    { disableLogTypes =
        Core.Nothing,
      enableLogTypes = Core.Nothing
    }

-- | The list of log types to disable.
--
-- /Note:/ Consider using 'disableLogTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clecDisableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Core.Maybe [Types.String])
clecDisableLogTypes = Lens.field @"disableLogTypes"
{-# DEPRECATED clecDisableLogTypes "Use generic-lens or generic-optics with 'disableLogTypes' instead." #-}

-- | The list of log types to enable.
--
-- /Note:/ Consider using 'enableLogTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clecEnableLogTypes :: Lens.Lens' CloudwatchLogsExportConfiguration (Core.Maybe [Types.String])
clecEnableLogTypes = Lens.field @"enableLogTypes"
{-# DEPRECATED clecEnableLogTypes "Use generic-lens or generic-optics with 'enableLogTypes' instead." #-}
