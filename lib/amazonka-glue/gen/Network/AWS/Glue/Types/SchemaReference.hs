{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.SchemaReference
  ( SchemaReference (..)
  -- * Smart constructor
  , mkSchemaReference
  -- * Lenses
  , srSchemaId
  , srSchemaVersionId
  , srSchemaVersionNumber
  ) where

import qualified Network.AWS.Glue.Types.SchemaId as Types
import qualified Network.AWS.Glue.Types.SchemaVersionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that references a schema stored in the AWS Glue Schema Registry.
--
-- /See:/ 'mkSchemaReference' smart constructor.
data SchemaReference = SchemaReference'
  { schemaId :: Core.Maybe Types.SchemaId
    -- ^ A structure that contains schema identity fields. Either this or the @SchemaVersionId@ has to be provided.
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The unique ID assigned to a version of the schema. Either this or the @SchemaId@ has to be provided.
  , schemaVersionNumber :: Core.Maybe Core.Natural
    -- ^ The version number of the schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaReference' value with any optional fields omitted.
mkSchemaReference
    :: SchemaReference
mkSchemaReference
  = SchemaReference'{schemaId = Core.Nothing,
                     schemaVersionId = Core.Nothing, schemaVersionNumber = Core.Nothing}

-- | A structure that contains schema identity fields. Either this or the @SchemaVersionId@ has to be provided.
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSchemaId :: Lens.Lens' SchemaReference (Core.Maybe Types.SchemaId)
srSchemaId = Lens.field @"schemaId"
{-# INLINEABLE srSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | The unique ID assigned to a version of the schema. Either this or the @SchemaId@ has to be provided.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSchemaVersionId :: Lens.Lens' SchemaReference (Core.Maybe Types.SchemaVersionId)
srSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE srSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSchemaVersionNumber :: Lens.Lens' SchemaReference (Core.Maybe Core.Natural)
srSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# INLINEABLE srSchemaVersionNumber #-}
{-# DEPRECATED schemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead"  #-}

instance Core.FromJSON SchemaReference where
        toJSON SchemaReference{..}
          = Core.object
              (Core.catMaybes
                 [("SchemaId" Core..=) Core.<$> schemaId,
                  ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
                  ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber])

instance Core.FromJSON SchemaReference where
        parseJSON
          = Core.withObject "SchemaReference" Core.$
              \ x ->
                SchemaReference' Core.<$>
                  (x Core..:? "SchemaId") Core.<*> x Core..:? "SchemaVersionId"
                    Core.<*> x Core..:? "SchemaVersionNumber"
