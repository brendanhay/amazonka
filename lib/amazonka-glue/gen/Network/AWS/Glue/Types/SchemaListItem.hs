{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaListItem
  ( SchemaListItem (..),

    -- * Smart constructor
    mkSchemaListItem,

    -- * Lenses
    sliCreatedTime,
    sliDescription,
    sliRegistryName,
    sliSchemaArn,
    sliSchemaName,
    sliSchemaStatus,
    sliUpdatedTime,
  )
where

import qualified Network.AWS.Glue.Types.CreatedTime as Types
import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.GlueResourceArn as Types
import qualified Network.AWS.Glue.Types.RegistryName as Types
import qualified Network.AWS.Glue.Types.SchemaName as Types
import qualified Network.AWS.Glue.Types.SchemaStatus as Types
import qualified Network.AWS.Glue.Types.UpdatedTimestamp as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains minimal details for a schema.
--
-- /See:/ 'mkSchemaListItem' smart constructor.
data SchemaListItem = SchemaListItem'
  { -- | The date and time that a schema was created.
    createdTime :: Core.Maybe Types.CreatedTime,
    -- | A description for the schema.
    description :: Core.Maybe Types.DescriptionString,
    -- | the name of the registry where the schema resides.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The Amazon Resource Name (ARN) for the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name of the schema.
    schemaName :: Core.Maybe Types.SchemaName,
    -- | The status of the schema.
    schemaStatus :: Core.Maybe Types.SchemaStatus,
    -- | The date and time that a schema was updated.
    updatedTime :: Core.Maybe Types.UpdatedTimestamp
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaListItem' value with any optional fields omitted.
mkSchemaListItem ::
  SchemaListItem
mkSchemaListItem =
  SchemaListItem'
    { createdTime = Core.Nothing,
      description = Core.Nothing,
      registryName = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaName = Core.Nothing,
      schemaStatus = Core.Nothing,
      updatedTime = Core.Nothing
    }

-- | The date and time that a schema was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sliCreatedTime :: Lens.Lens' SchemaListItem (Core.Maybe Types.CreatedTime)
sliCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED sliCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | A description for the schema.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sliDescription :: Lens.Lens' SchemaListItem (Core.Maybe Types.DescriptionString)
sliDescription = Lens.field @"description"
{-# DEPRECATED sliDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | the name of the registry where the schema resides.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sliRegistryName :: Lens.Lens' SchemaListItem (Core.Maybe Types.RegistryName)
sliRegistryName = Lens.field @"registryName"
{-# DEPRECATED sliRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) for the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sliSchemaArn :: Lens.Lens' SchemaListItem (Core.Maybe Types.GlueResourceArn)
sliSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED sliSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sliSchemaName :: Lens.Lens' SchemaListItem (Core.Maybe Types.SchemaName)
sliSchemaName = Lens.field @"schemaName"
{-# DEPRECATED sliSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'schemaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sliSchemaStatus :: Lens.Lens' SchemaListItem (Core.Maybe Types.SchemaStatus)
sliSchemaStatus = Lens.field @"schemaStatus"
{-# DEPRECATED sliSchemaStatus "Use generic-lens or generic-optics with 'schemaStatus' instead." #-}

-- | The date and time that a schema was updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sliUpdatedTime :: Lens.Lens' SchemaListItem (Core.Maybe Types.UpdatedTimestamp)
sliUpdatedTime = Lens.field @"updatedTime"
{-# DEPRECATED sliUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

instance Core.FromJSON SchemaListItem where
  parseJSON =
    Core.withObject "SchemaListItem" Core.$
      \x ->
        SchemaListItem'
          Core.<$> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "RegistryName")
          Core.<*> (x Core..:? "SchemaArn")
          Core.<*> (x Core..:? "SchemaName")
          Core.<*> (x Core..:? "SchemaStatus")
          Core.<*> (x Core..:? "UpdatedTime")
