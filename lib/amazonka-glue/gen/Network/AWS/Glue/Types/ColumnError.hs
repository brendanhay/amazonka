{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnError
  ( ColumnError (..),

    -- * Smart constructor
    mkColumnError,

    -- * Lenses
    ceColumnName,
    ceError,
  )
where

import qualified Network.AWS.Glue.Types.ErrorDetail as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encapsulates a column name that failed and the reason for failure.
--
-- /See:/ 'mkColumnError' smart constructor.
data ColumnError = ColumnError'
  { -- | The name of the column that failed.
    columnName :: Core.Maybe Types.NameString,
    -- | An error message with the reason for the failure of an operation.
    error :: Core.Maybe Types.ErrorDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ColumnError' value with any optional fields omitted.
mkColumnError ::
  ColumnError
mkColumnError =
  ColumnError' {columnName = Core.Nothing, error = Core.Nothing}

-- | The name of the column that failed.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceColumnName :: Lens.Lens' ColumnError (Core.Maybe Types.NameString)
ceColumnName = Lens.field @"columnName"
{-# DEPRECATED ceColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

-- | An error message with the reason for the failure of an operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceError :: Lens.Lens' ColumnError (Core.Maybe Types.ErrorDetail)
ceError = Lens.field @"error"
{-# DEPRECATED ceError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Core.FromJSON ColumnError where
  parseJSON =
    Core.withObject "ColumnError" Core.$
      \x ->
        ColumnError'
          Core.<$> (x Core..:? "ColumnName") Core.<*> (x Core..:? "Error")
