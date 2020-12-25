{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
  ( TypedLinkSchemaAndFacetName (..),

    -- * Smart constructor
    mkTypedLinkSchemaAndFacetName,

    -- * Lenses
    tlsafnSchemaArn,
    tlsafnTypedLinkName,
  )
where

import qualified Network.AWS.CloudDirectory.Types.Arn as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.
--
-- /See:/ 'mkTypedLinkSchemaAndFacetName' smart constructor.
data TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaArn :: Types.Arn,
    -- | The unique name of the typed link facet.
    typedLinkName :: Types.TypedLinkName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TypedLinkSchemaAndFacetName' value with any optional fields omitted.
mkTypedLinkSchemaAndFacetName ::
  -- | 'schemaArn'
  Types.Arn ->
  -- | 'typedLinkName'
  Types.TypedLinkName ->
  TypedLinkSchemaAndFacetName
mkTypedLinkSchemaAndFacetName schemaArn typedLinkName =
  TypedLinkSchemaAndFacetName' {schemaArn, typedLinkName}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsafnSchemaArn :: Lens.Lens' TypedLinkSchemaAndFacetName Types.Arn
tlsafnSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED tlsafnSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'typedLinkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsafnTypedLinkName :: Lens.Lens' TypedLinkSchemaAndFacetName Types.TypedLinkName
tlsafnTypedLinkName = Lens.field @"typedLinkName"
{-# DEPRECATED tlsafnTypedLinkName "Use generic-lens or generic-optics with 'typedLinkName' instead." #-}

instance Core.FromJSON TypedLinkSchemaAndFacetName where
  toJSON TypedLinkSchemaAndFacetName {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaArn" Core..= schemaArn),
            Core.Just ("TypedLinkName" Core..= typedLinkName)
          ]
      )

instance Core.FromJSON TypedLinkSchemaAndFacetName where
  parseJSON =
    Core.withObject "TypedLinkSchemaAndFacetName" Core.$
      \x ->
        TypedLinkSchemaAndFacetName'
          Core.<$> (x Core..: "SchemaArn") Core.<*> (x Core..: "TypedLinkName")
