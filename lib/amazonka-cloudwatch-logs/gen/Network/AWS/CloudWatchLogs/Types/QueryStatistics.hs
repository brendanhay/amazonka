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
    qsRecordsScanned,
    qsBytesScanned,
    qsRecordsMatched,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned.
--
-- /See:/ 'mkQueryStatistics' smart constructor.
data QueryStatistics = QueryStatistics'
  { -- | The total number of log events scanned during the query.
    recordsScanned :: Lude.Maybe Lude.Double,
    -- | The total number of bytes in the log events scanned during the query.
    bytesScanned :: Lude.Maybe Lude.Double,
    -- | The number of log events that matched the query string.
    recordsMatched :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryStatistics' with the minimum fields required to make a request.
--
-- * 'recordsScanned' - The total number of log events scanned during the query.
-- * 'bytesScanned' - The total number of bytes in the log events scanned during the query.
-- * 'recordsMatched' - The number of log events that matched the query string.
mkQueryStatistics ::
  QueryStatistics
mkQueryStatistics =
  QueryStatistics'
    { recordsScanned = Lude.Nothing,
      bytesScanned = Lude.Nothing,
      recordsMatched = Lude.Nothing
    }

-- | The total number of log events scanned during the query.
--
-- /Note:/ Consider using 'recordsScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsRecordsScanned :: Lens.Lens' QueryStatistics (Lude.Maybe Lude.Double)
qsRecordsScanned = Lens.lens (recordsScanned :: QueryStatistics -> Lude.Maybe Lude.Double) (\s a -> s {recordsScanned = a} :: QueryStatistics)
{-# DEPRECATED qsRecordsScanned "Use generic-lens or generic-optics with 'recordsScanned' instead." #-}

-- | The total number of bytes in the log events scanned during the query.
--
-- /Note:/ Consider using 'bytesScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsBytesScanned :: Lens.Lens' QueryStatistics (Lude.Maybe Lude.Double)
qsBytesScanned = Lens.lens (bytesScanned :: QueryStatistics -> Lude.Maybe Lude.Double) (\s a -> s {bytesScanned = a} :: QueryStatistics)
{-# DEPRECATED qsBytesScanned "Use generic-lens or generic-optics with 'bytesScanned' instead." #-}

-- | The number of log events that matched the query string.
--
-- /Note:/ Consider using 'recordsMatched' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsRecordsMatched :: Lens.Lens' QueryStatistics (Lude.Maybe Lude.Double)
qsRecordsMatched = Lens.lens (recordsMatched :: QueryStatistics -> Lude.Maybe Lude.Double) (\s a -> s {recordsMatched = a} :: QueryStatistics)
{-# DEPRECATED qsRecordsMatched "Use generic-lens or generic-optics with 'recordsMatched' instead." #-}

instance Lude.FromJSON QueryStatistics where
  parseJSON =
    Lude.withObject
      "QueryStatistics"
      ( \x ->
          QueryStatistics'
            Lude.<$> (x Lude..:? "recordsScanned")
            Lude.<*> (x Lude..:? "bytesScanned")
            Lude.<*> (x Lude..:? "recordsMatched")
      )
