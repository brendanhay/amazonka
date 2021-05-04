{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsError where

import Network.AWS.Glue.Types.ColumnStatistics
import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Encapsulates a @ColumnStatistics@ object that failed and the reason for
-- failure.
--
-- /See:/ 'newColumnStatisticsError' smart constructor.
data ColumnStatisticsError = ColumnStatisticsError'
  { -- | The @ColumnStatistics@ of the column.
    columnStatistics :: Prelude.Maybe ColumnStatistics,
    -- | An error message with the reason for the failure of an operation.
    error :: Prelude.Maybe ErrorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ColumnStatisticsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnStatistics', 'columnStatisticsError_columnStatistics' - The @ColumnStatistics@ of the column.
--
-- 'error', 'columnStatisticsError_error' - An error message with the reason for the failure of an operation.
newColumnStatisticsError ::
  ColumnStatisticsError
newColumnStatisticsError =
  ColumnStatisticsError'
    { columnStatistics =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The @ColumnStatistics@ of the column.
columnStatisticsError_columnStatistics :: Lens.Lens' ColumnStatisticsError (Prelude.Maybe ColumnStatistics)
columnStatisticsError_columnStatistics = Lens.lens (\ColumnStatisticsError' {columnStatistics} -> columnStatistics) (\s@ColumnStatisticsError' {} a -> s {columnStatistics = a} :: ColumnStatisticsError)

-- | An error message with the reason for the failure of an operation.
columnStatisticsError_error :: Lens.Lens' ColumnStatisticsError (Prelude.Maybe ErrorDetail)
columnStatisticsError_error = Lens.lens (\ColumnStatisticsError' {error} -> error) (\s@ColumnStatisticsError' {} a -> s {error = a} :: ColumnStatisticsError)

instance Prelude.FromJSON ColumnStatisticsError where
  parseJSON =
    Prelude.withObject
      "ColumnStatisticsError"
      ( \x ->
          ColumnStatisticsError'
            Prelude.<$> (x Prelude..:? "ColumnStatistics")
            Prelude.<*> (x Prelude..:? "Error")
      )

instance Prelude.Hashable ColumnStatisticsError

instance Prelude.NFData ColumnStatisticsError
