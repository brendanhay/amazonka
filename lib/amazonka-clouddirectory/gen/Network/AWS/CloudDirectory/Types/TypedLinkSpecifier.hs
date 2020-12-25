{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
  ( TypedLinkSpecifier (..),

    -- * Smart constructor
    mkTypedLinkSpecifier,

    -- * Lenses
    tlsTypedLinkFacet,
    tlsSourceObjectReference,
    tlsTargetObjectReference,
    tlsIdentityAttributeValues,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeNameAndValue as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The 'AttachTypedLink' API returns a typed link specifier while the 'DetachTypedLink' API accepts one as input. Similarly, the 'ListIncomingTypedLinks' and 'ListOutgoingTypedLinks' API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.
--
-- /See:/ 'mkTypedLinkSpecifier' smart constructor.
data TypedLinkSpecifier = TypedLinkSpecifier'
  { -- | Identifies the typed link facet that is associated with the typed link.
    typedLinkFacet :: Types.TypedLinkSchemaAndFacetName,
    -- | Identifies the source object that the typed link will attach to.
    sourceObjectReference :: Types.ObjectReference,
    -- | Identifies the target object that the typed link will attach to.
    targetObjectReference :: Types.ObjectReference,
    -- | Identifies the attribute value to update.
    identityAttributeValues :: [Types.AttributeNameAndValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TypedLinkSpecifier' value with any optional fields omitted.
mkTypedLinkSpecifier ::
  -- | 'typedLinkFacet'
  Types.TypedLinkSchemaAndFacetName ->
  -- | 'sourceObjectReference'
  Types.ObjectReference ->
  -- | 'targetObjectReference'
  Types.ObjectReference ->
  TypedLinkSpecifier
mkTypedLinkSpecifier
  typedLinkFacet
  sourceObjectReference
  targetObjectReference =
    TypedLinkSpecifier'
      { typedLinkFacet,
        sourceObjectReference,
        targetObjectReference,
        identityAttributeValues = Core.mempty
      }

-- | Identifies the typed link facet that is associated with the typed link.
--
-- /Note:/ Consider using 'typedLinkFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsTypedLinkFacet :: Lens.Lens' TypedLinkSpecifier Types.TypedLinkSchemaAndFacetName
tlsTypedLinkFacet = Lens.field @"typedLinkFacet"
{-# DEPRECATED tlsTypedLinkFacet "Use generic-lens or generic-optics with 'typedLinkFacet' instead." #-}

-- | Identifies the source object that the typed link will attach to.
--
-- /Note:/ Consider using 'sourceObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsSourceObjectReference :: Lens.Lens' TypedLinkSpecifier Types.ObjectReference
tlsSourceObjectReference = Lens.field @"sourceObjectReference"
{-# DEPRECATED tlsSourceObjectReference "Use generic-lens or generic-optics with 'sourceObjectReference' instead." #-}

-- | Identifies the target object that the typed link will attach to.
--
-- /Note:/ Consider using 'targetObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsTargetObjectReference :: Lens.Lens' TypedLinkSpecifier Types.ObjectReference
tlsTargetObjectReference = Lens.field @"targetObjectReference"
{-# DEPRECATED tlsTargetObjectReference "Use generic-lens or generic-optics with 'targetObjectReference' instead." #-}

-- | Identifies the attribute value to update.
--
-- /Note:/ Consider using 'identityAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsIdentityAttributeValues :: Lens.Lens' TypedLinkSpecifier [Types.AttributeNameAndValue]
tlsIdentityAttributeValues = Lens.field @"identityAttributeValues"
{-# DEPRECATED tlsIdentityAttributeValues "Use generic-lens or generic-optics with 'identityAttributeValues' instead." #-}

instance Core.FromJSON TypedLinkSpecifier where
  toJSON TypedLinkSpecifier {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TypedLinkFacet" Core..= typedLinkFacet),
            Core.Just ("SourceObjectReference" Core..= sourceObjectReference),
            Core.Just ("TargetObjectReference" Core..= targetObjectReference),
            Core.Just
              ("IdentityAttributeValues" Core..= identityAttributeValues)
          ]
      )

instance Core.FromJSON TypedLinkSpecifier where
  parseJSON =
    Core.withObject "TypedLinkSpecifier" Core.$
      \x ->
        TypedLinkSpecifier'
          Core.<$> (x Core..: "TypedLinkFacet")
          Core.<*> (x Core..: "SourceObjectReference")
          Core.<*> (x Core..: "TargetObjectReference")
          Core.<*> (x Core..:? "IdentityAttributeValues" Core..!= Core.mempty)
