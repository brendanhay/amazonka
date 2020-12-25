{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsError
  ( ColumnStatisticsError (..),

    -- * Smart constructor
    mkColumnStatisticsError,

    -- * Lenses
    cseColumnStatistics,
    cseError,
  )
where

import qualified Network.AWS.Glue.Types.ColumnStatistics as Types
import qualified Network.AWS.Glue.Types.ErrorDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encapsulates a @ColumnStatistics@ object that failed and the reason for failure.
--
-- /See:/ 'mkColumnStatisticsError' smart constructor.
data ColumnStatisticsError = ColumnStatisticsError'
  { -- | The @ColumnStatistics@ of the column.
    columnStatistics :: Core.Maybe Types.ColumnStatistics,
    -- | An error message with the reason for the failure of an operation.
    error :: Core.Maybe Types.ErrorDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ColumnStatisticsError' value with any optional fields omitted.
mkColumnStatisticsError ::
  ColumnStatisticsError
mkColumnStatisticsError =
  ColumnStatisticsError'
    { columnStatistics = Core.Nothing,
      error = Core.Nothing
    }

-- | The @ColumnStatistics@ of the column.
--
-- /Note:/ Consider using 'columnStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseColumnStatistics :: Lens.Lens' ColumnStatisticsError (Core.Maybe Types.ColumnStatistics)
cseColumnStatistics = Lens.field @"columnStatistics"
{-# DEPRECATED cseColumnStatistics "Use generic-lens or generic-optics with 'columnStatistics' instead." #-}

-- | An error message with the reason for the failure of an operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseError :: Lens.Lens' ColumnStatisticsError (Core.Maybe Types.ErrorDetail)
cseError = Lens.field @"error"
{-# DEPRECATED cseError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromJSON ColumnStatisticsError where
  parseJSON =
    Core.withObject "ColumnStatisticsError" Core.$
      \x ->
        ColumnStatisticsError'
          Core.<$> (x Core..:? "ColumnStatistics") Core.<*> (x Core..:? "Error")
