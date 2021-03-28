{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SchemaExtensionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.SchemaExtensionInfo
  ( SchemaExtensionInfo (..)
  -- * Smart constructor
  , mkSchemaExtensionInfo
  -- * Lenses
  , seiDescription
  , seiDirectoryId
  , seiEndDateTime
  , seiSchemaExtensionId
  , seiSchemaExtensionStatus
  , seiSchemaExtensionStatusReason
  , seiStartDateTime
  ) where

import qualified Network.AWS.DirectoryService.Types.Description as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.SchemaExtensionId as Types
import qualified Network.AWS.DirectoryService.Types.SchemaExtensionStatus as Types
import qualified Network.AWS.DirectoryService.Types.SchemaExtensionStatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a schema extension.
--
-- /See:/ 'mkSchemaExtensionInfo' smart constructor.
data SchemaExtensionInfo = SchemaExtensionInfo'
  { description :: Core.Maybe Types.Description
    -- ^ A description of the schema extension.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the directory to which the schema extension is applied.
  , endDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the schema extension was completed.
  , schemaExtensionId :: Core.Maybe Types.SchemaExtensionId
    -- ^ The identifier of the schema extension.
  , schemaExtensionStatus :: Core.Maybe Types.SchemaExtensionStatus
    -- ^ The current status of the schema extension.
  , schemaExtensionStatusReason :: Core.Maybe Types.SchemaExtensionStatusReason
    -- ^ The reason for the @SchemaExtensionStatus@ .
  , startDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the schema extension started being applied to the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SchemaExtensionInfo' value with any optional fields omitted.
mkSchemaExtensionInfo
    :: SchemaExtensionInfo
mkSchemaExtensionInfo
  = SchemaExtensionInfo'{description = Core.Nothing,
                         directoryId = Core.Nothing, endDateTime = Core.Nothing,
                         schemaExtensionId = Core.Nothing,
                         schemaExtensionStatus = Core.Nothing,
                         schemaExtensionStatusReason = Core.Nothing,
                         startDateTime = Core.Nothing}

-- | A description of the schema extension.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiDescription :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Types.Description)
seiDescription = Lens.field @"description"
{-# INLINEABLE seiDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier of the directory to which the schema extension is applied.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiDirectoryId :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Types.DirectoryId)
seiDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE seiDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The date and time that the schema extension was completed.
--
-- /Note:/ Consider using 'endDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiEndDateTime :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.NominalDiffTime)
seiEndDateTime = Lens.field @"endDateTime"
{-# INLINEABLE seiEndDateTime #-}
{-# DEPRECATED endDateTime "Use generic-lens or generic-optics with 'endDateTime' instead"  #-}

-- | The identifier of the schema extension.
--
-- /Note:/ Consider using 'schemaExtensionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiSchemaExtensionId :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Types.SchemaExtensionId)
seiSchemaExtensionId = Lens.field @"schemaExtensionId"
{-# INLINEABLE seiSchemaExtensionId #-}
{-# DEPRECATED schemaExtensionId "Use generic-lens or generic-optics with 'schemaExtensionId' instead"  #-}

-- | The current status of the schema extension.
--
-- /Note:/ Consider using 'schemaExtensionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiSchemaExtensionStatus :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Types.SchemaExtensionStatus)
seiSchemaExtensionStatus = Lens.field @"schemaExtensionStatus"
{-# INLINEABLE seiSchemaExtensionStatus #-}
{-# DEPRECATED schemaExtensionStatus "Use generic-lens or generic-optics with 'schemaExtensionStatus' instead"  #-}

-- | The reason for the @SchemaExtensionStatus@ .
--
-- /Note:/ Consider using 'schemaExtensionStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiSchemaExtensionStatusReason :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Types.SchemaExtensionStatusReason)
seiSchemaExtensionStatusReason = Lens.field @"schemaExtensionStatusReason"
{-# INLINEABLE seiSchemaExtensionStatusReason #-}
{-# DEPRECATED schemaExtensionStatusReason "Use generic-lens or generic-optics with 'schemaExtensionStatusReason' instead"  #-}

-- | The date and time that the schema extension started being applied to the directory.
--
-- /Note:/ Consider using 'startDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seiStartDateTime :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.NominalDiffTime)
seiStartDateTime = Lens.field @"startDateTime"
{-# INLINEABLE seiStartDateTime #-}
{-# DEPRECATED startDateTime "Use generic-lens or generic-optics with 'startDateTime' instead"  #-}

instance Core.FromJSON SchemaExtensionInfo where
        parseJSON
          = Core.withObject "SchemaExtensionInfo" Core.$
              \ x ->
                SchemaExtensionInfo' Core.<$>
                  (x Core..:? "Description") Core.<*> x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "EndDateTime"
                    Core.<*> x Core..:? "SchemaExtensionId"
                    Core.<*> x Core..:? "SchemaExtensionStatus"
                    Core.<*> x Core..:? "SchemaExtensionStatusReason"
                    Core.<*> x Core..:? "StartDateTime"
