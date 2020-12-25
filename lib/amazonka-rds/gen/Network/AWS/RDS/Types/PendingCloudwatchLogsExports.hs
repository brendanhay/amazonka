{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingCloudwatchLogsExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.PendingCloudwatchLogsExports
  ( PendingCloudwatchLogsExports (..),

    -- * Smart constructor
    mkPendingCloudwatchLogsExports,

    -- * Lenses
    pcleLogTypesToDisable,
    pcleLogTypesToEnable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | A list of the log types whose configuration is still pending. In other words, these log types are in the process of being activated or deactivated.
--
-- /See:/ 'mkPendingCloudwatchLogsExports' smart constructor.
data PendingCloudwatchLogsExports = PendingCloudwatchLogsExports'
  { -- | Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
    logTypesToDisable :: Core.Maybe [Types.String],
    -- | Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
    logTypesToEnable :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingCloudwatchLogsExports' value with any optional fields omitted.
mkPendingCloudwatchLogsExports ::
  PendingCloudwatchLogsExports
mkPendingCloudwatchLogsExports =
  PendingCloudwatchLogsExports'
    { logTypesToDisable = Core.Nothing,
      logTypesToEnable = Core.Nothing
    }

-- | Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
--
-- /Note:/ Consider using 'logTypesToDisable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcleLogTypesToDisable :: Lens.Lens' PendingCloudwatchLogsExports (Core.Maybe [Types.String])
pcleLogTypesToDisable = Lens.field @"logTypesToDisable"
{-# DEPRECATED pcleLogTypesToDisable "Use generic-lens or generic-optics with 'logTypesToDisable' instead." #-}

-- | Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
--
-- /Note:/ Consider using 'logTypesToEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcleLogTypesToEnable :: Lens.Lens' PendingCloudwatchLogsExports (Core.Maybe [Types.String])
pcleLogTypesToEnable = Lens.field @"logTypesToEnable"
{-# DEPRECATED pcleLogTypesToEnable "Use generic-lens or generic-optics with 'logTypesToEnable' instead." #-}

instance Core.FromXML PendingCloudwatchLogsExports where
  parseXML x =
    PendingCloudwatchLogsExports'
      Core.<$> ( x Core..@? "LogTypesToDisable"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "LogTypesToEnable"
                   Core..<@> Core.parseXMLList "member"
               )
