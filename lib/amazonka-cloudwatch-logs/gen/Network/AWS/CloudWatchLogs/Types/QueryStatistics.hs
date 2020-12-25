{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryStatistics
  ( QueryStatistics (..),

    -- * Smart constructor
    mkQueryStatistics,

    -- * Lenses
    qsBytesScanned,
    qsRecordsMatched,
    qsRecordsScanned,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned.
--
-- /See:/ 'mkQueryStatistics' smart constructor.
data QueryStatistics = QueryStatistics'
  { -- | The total number of bytes in the log events scanned during the query.
    bytesScanned :: Core.Maybe Core.Double,
    -- | The number of log events that matched the query string.
    recordsMatched :: Core.Maybe Core.Double,
    -- | The total number of log events scanned during the query.
    recordsScanned :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryStatistics' value with any optional fields omitted.
mkQueryStatistics ::
  QueryStatistics
mkQueryStatistics =
  QueryStatistics'
    { bytesScanned = Core.Nothing,
      recordsMatched = Core.Nothing,
      recordsScanned = Core.Nothing
    }

-- | The total number of bytes in the log events scanned during the query.
--
-- /Note:/ Consider using 'bytesScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsBytesScanned :: Lens.Lens' QueryStatistics (Core.Maybe Core.Double)
qsBytesScanned = Lens.field @"bytesScanned"
{-# DEPRECATED qsBytesScanned "Use generic-lens or generic-optics with 'bytesScanned' instead." #-}

-- | The number of log events that matched the query string.
--
-- /Note:/ Consider using 'recordsMatched' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsRecordsMatched :: Lens.Lens' QueryStatistics (Core.Maybe Core.Double)
qsRecordsMatched = Lens.field @"recordsMatched"
{-# DEPRECATED qsRecordsMatched "Use generic-lens or generic-optics with 'recordsMatched' instead." #-}

-- | The total number of log events scanned during the query.
--
-- /Note:/ Consider using 'recordsScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsRecordsScanned :: Lens.Lens' QueryStatistics (Core.Maybe Core.Double)
qsRecordsScanned = Lens.field @"recordsScanned"
{-# DEPRECATED qsRecordsScanned "Use generic-lens or generic-optics with 'recordsScanned' instead." #-}

instance Core.FromJSON QueryStatistics where
  parseJSON =
    Core.withObject "QueryStatistics" Core.$
      \x ->
        QueryStatistics'
          Core.<$> (x Core..:? "bytesScanned")
          Core.<*> (x Core..:? "recordsMatched")
          Core.<*> (x Core..:? "recordsScanned")
