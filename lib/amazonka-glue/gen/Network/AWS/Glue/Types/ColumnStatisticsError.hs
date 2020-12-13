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
    cseError,
    cseColumnStatistics,
  )
where

import Network.AWS.Glue.Types.ColumnStatistics
import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encapsulates a @ColumnStatistics@ object that failed and the reason for failure.
--
-- /See:/ 'mkColumnStatisticsError' smart constructor.
data ColumnStatisticsError = ColumnStatisticsError'
  { -- | An error message with the reason for the failure of an operation.
    error :: Lude.Maybe ErrorDetail,
    -- | The @ColumnStatistics@ of the column.
    columnStatistics :: Lude.Maybe ColumnStatistics
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ColumnStatisticsError' with the minimum fields required to make a request.
--
-- * 'error' - An error message with the reason for the failure of an operation.
-- * 'columnStatistics' - The @ColumnStatistics@ of the column.
mkColumnStatisticsError ::
  ColumnStatisticsError
mkColumnStatisticsError =
  ColumnStatisticsError'
    { error = Lude.Nothing,
      columnStatistics = Lude.Nothing
    }

-- | An error message with the reason for the failure of an operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseError :: Lens.Lens' ColumnStatisticsError (Lude.Maybe ErrorDetail)
cseError = Lens.lens (error :: ColumnStatisticsError -> Lude.Maybe ErrorDetail) (\s a -> s {error = a} :: ColumnStatisticsError)
{-# DEPRECATED cseError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The @ColumnStatistics@ of the column.
--
-- /Note:/ Consider using 'columnStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cseColumnStatistics :: Lens.Lens' ColumnStatisticsError (Lude.Maybe ColumnStatistics)
cseColumnStatistics = Lens.lens (columnStatistics :: ColumnStatisticsError -> Lude.Maybe ColumnStatistics) (\s a -> s {columnStatistics = a} :: ColumnStatisticsError)
{-# DEPRECATED cseColumnStatistics "Use generic-lens or generic-optics with 'columnStatistics' instead." #-}

instance Lude.FromJSON ColumnStatisticsError where
  parseJSON =
    Lude.withObject
      "ColumnStatisticsError"
      ( \x ->
          ColumnStatisticsError'
            Lude.<$> (x Lude..:? "Error") Lude.<*> (x Lude..:? "ColumnStatistics")
      )
