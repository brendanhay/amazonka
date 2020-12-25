{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionListItem
  ( SchemaVersionListItem (..),

    -- * Smart constructor
    mkSchemaVersionListItem,

    -- * Lenses
    svliCreatedTime,
    svliSchemaArn,
    svliSchemaVersionId,
    svliStatus,
    svliVersionNumber,
  )
where

import qualified Network.AWS.Glue.Types.CreatedTime as Types
import qualified Network.AWS.Glue.Types.GlueResourceArn as Types
import qualified Network.AWS.Glue.Types.SchemaVersionId as Types
import qualified Network.AWS.Glue.Types.SchemaVersionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object containing the details about a schema version.
--
-- /See:/ 'mkSchemaVersionListItem' smart constructor.
data SchemaVersionListItem = SchemaVersionListItem'
  { -- | The date and time the schema version was created.
    createdTime :: Core.Maybe Types.CreatedTime,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The unique identifier of the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The status of the schema version.
    status :: Core.Maybe Types.SchemaVersionStatus,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaVersionListItem' value with any optional fields omitted.
mkSchemaVersionListItem ::
  SchemaVersionListItem
mkSchemaVersionListItem =
  SchemaVersionListItem'
    { createdTime = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaVersionId = Core.Nothing,
      status = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | The date and time the schema version was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliCreatedTime :: Lens.Lens' SchemaVersionListItem (Core.Maybe Types.CreatedTime)
svliCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED svliCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliSchemaArn :: Lens.Lens' SchemaVersionListItem (Core.Maybe Types.GlueResourceArn)
svliSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED svliSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The unique identifier of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliSchemaVersionId :: Lens.Lens' SchemaVersionListItem (Core.Maybe Types.SchemaVersionId)
svliSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED svliSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliStatus :: Lens.Lens' SchemaVersionListItem (Core.Maybe Types.SchemaVersionStatus)
svliStatus = Lens.field @"status"
{-# DEPRECATED svliStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svliVersionNumber :: Lens.Lens' SchemaVersionListItem (Core.Maybe Core.Natural)
svliVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED svliVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.FromJSON SchemaVersionListItem where
  parseJSON =
    Core.withObject "SchemaVersionListItem" Core.$
      \x ->
        SchemaVersionListItem'
          Core.<$> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "SchemaArn")
          Core.<*> (x Core..:? "SchemaVersionId")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "VersionNumber")
