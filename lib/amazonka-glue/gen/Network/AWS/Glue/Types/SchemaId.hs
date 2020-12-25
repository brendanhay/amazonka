{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaId
  ( SchemaId (..),

    -- * Smart constructor
    mkSchemaId,

    -- * Lenses
    siRegistryName,
    siSchemaArn,
    siSchemaName,
  )
where

import qualified Network.AWS.Glue.Types.GlueResourceArn as Types
import qualified Network.AWS.Glue.Types.RegistryName as Types
import qualified Network.AWS.Glue.Types.SchemaName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkSchemaId' smart constructor.
data SchemaId = SchemaId'
  { registryName :: Core.Maybe Types.RegistryName,
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    schemaName :: Core.Maybe Types.SchemaName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaId' value with any optional fields omitted.
mkSchemaId ::
  SchemaId
mkSchemaId =
  SchemaId'
    { registryName = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaName = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRegistryName :: Lens.Lens' SchemaId (Core.Maybe Types.RegistryName)
siRegistryName = Lens.field @"registryName"
{-# DEPRECATED siRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSchemaArn :: Lens.Lens' SchemaId (Core.Maybe Types.GlueResourceArn)
siSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED siSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siSchemaName :: Lens.Lens' SchemaId (Core.Maybe Types.SchemaName)
siSchemaName = Lens.field @"schemaName"
{-# DEPRECATED siSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

instance Core.FromJSON SchemaId where
  toJSON SchemaId {..} =
    Core.object
      ( Core.catMaybes
          [ ("RegistryName" Core..=) Core.<$> registryName,
            ("SchemaArn" Core..=) Core.<$> schemaArn,
            ("SchemaName" Core..=) Core.<$> schemaName
          ]
      )

instance Core.FromJSON SchemaId where
  parseJSON =
    Core.withObject "SchemaId" Core.$
      \x ->
        SchemaId'
          Core.<$> (x Core..:? "RegistryName")
          Core.<*> (x Core..:? "SchemaArn")
          Core.<*> (x Core..:? "SchemaName")
