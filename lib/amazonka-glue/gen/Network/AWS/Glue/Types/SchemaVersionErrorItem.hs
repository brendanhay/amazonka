{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionErrorItem
  ( SchemaVersionErrorItem (..),

    -- * Smart constructor
    mkSchemaVersionErrorItem,

    -- * Lenses
    sveiErrorDetails,
    sveiVersionNumber,
  )
where

import qualified Network.AWS.Glue.Types.ErrorDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains the error details for an operation on a schema version.
--
-- /See:/ 'mkSchemaVersionErrorItem' smart constructor.
data SchemaVersionErrorItem = SchemaVersionErrorItem'
  { -- | The details of the error for the schema version.
    errorDetails :: Core.Maybe Types.ErrorDetails,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaVersionErrorItem' value with any optional fields omitted.
mkSchemaVersionErrorItem ::
  SchemaVersionErrorItem
mkSchemaVersionErrorItem =
  SchemaVersionErrorItem'
    { errorDetails = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | The details of the error for the schema version.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sveiErrorDetails :: Lens.Lens' SchemaVersionErrorItem (Core.Maybe Types.ErrorDetails)
sveiErrorDetails = Lens.field @"errorDetails"
{-# DEPRECATED sveiErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sveiVersionNumber :: Lens.Lens' SchemaVersionErrorItem (Core.Maybe Core.Natural)
sveiVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED sveiVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.FromJSON SchemaVersionErrorItem where
  parseJSON =
    Core.withObject "SchemaVersionErrorItem" Core.$
      \x ->
        SchemaVersionErrorItem'
          Core.<$> (x Core..:? "ErrorDetails") Core.<*> (x Core..:? "VersionNumber")
