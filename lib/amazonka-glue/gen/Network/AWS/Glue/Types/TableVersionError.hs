{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableVersionError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableVersionError
  ( TableVersionError (..),

    -- * Smart constructor
    mkTableVersionError,

    -- * Lenses
    tveErrorDetail,
    tveTableName,
    tveVersionId,
  )
where

import qualified Network.AWS.Glue.Types.ErrorDetail as Types
import qualified Network.AWS.Glue.Types.TableName as Types
import qualified Network.AWS.Glue.Types.VersionString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An error record for table-version operations.
--
-- /See:/ 'mkTableVersionError' smart constructor.
data TableVersionError = TableVersionError'
  { -- | The details about the error.
    errorDetail :: Core.Maybe Types.ErrorDetail,
    -- | The name of the table in question.
    tableName :: Core.Maybe Types.TableName,
    -- | The ID value of the version in question. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
    versionId :: Core.Maybe Types.VersionString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TableVersionError' value with any optional fields omitted.
mkTableVersionError ::
  TableVersionError
mkTableVersionError =
  TableVersionError'
    { errorDetail = Core.Nothing,
      tableName = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The details about the error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tveErrorDetail :: Lens.Lens' TableVersionError (Core.Maybe Types.ErrorDetail)
tveErrorDetail = Lens.field @"errorDetail"
{-# DEPRECATED tveErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

-- | The name of the table in question.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tveTableName :: Lens.Lens' TableVersionError (Core.Maybe Types.TableName)
tveTableName = Lens.field @"tableName"
{-# DEPRECATED tveTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The ID value of the version in question. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tveVersionId :: Lens.Lens' TableVersionError (Core.Maybe Types.VersionString)
tveVersionId = Lens.field @"versionId"
{-# DEPRECATED tveVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.FromJSON TableVersionError where
  parseJSON =
    Core.withObject "TableVersionError" Core.$
      \x ->
        TableVersionError'
          Core.<$> (x Core..:? "ErrorDetail")
          Core.<*> (x Core..:? "TableName")
          Core.<*> (x Core..:? "VersionId")
