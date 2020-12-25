{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableError
  ( TableError (..),

    -- * Smart constructor
    mkTableError,

    -- * Lenses
    teErrorDetail,
    teTableName,
  )
where

import qualified Network.AWS.Glue.Types.ErrorDetail as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An error record for table operations.
--
-- /See:/ 'mkTableError' smart constructor.
data TableError = TableError'
  { -- | The details about the error.
    errorDetail :: Core.Maybe Types.ErrorDetail,
    -- | The name of the table. For Hive compatibility, this must be entirely lowercase.
    tableName :: Core.Maybe Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TableError' value with any optional fields omitted.
mkTableError ::
  TableError
mkTableError =
  TableError' {errorDetail = Core.Nothing, tableName = Core.Nothing}

-- | The details about the error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teErrorDetail :: Lens.Lens' TableError (Core.Maybe Types.ErrorDetail)
teErrorDetail = Lens.field @"errorDetail"
{-# DEPRECATED teErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

-- | The name of the table. For Hive compatibility, this must be entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teTableName :: Lens.Lens' TableError (Core.Maybe Types.NameString)
teTableName = Lens.field @"tableName"
{-# DEPRECATED teTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON TableError where
  parseJSON =
    Core.withObject "TableError" Core.$
      \x ->
        TableError'
          Core.<$> (x Core..:? "ErrorDetail") Core.<*> (x Core..:? "TableName")
