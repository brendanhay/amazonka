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

import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The 'AttachTypedLink' API returns a typed link specifier while the 'DetachTypedLink' API accepts one as input. Similarly, the 'ListIncomingTypedLinks' and 'ListOutgoingTypedLinks' API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.
--
-- /See:/ 'mkTypedLinkSpecifier' smart constructor.
data TypedLinkSpecifier = TypedLinkSpecifier'
  { typedLinkFacet ::
      TypedLinkSchemaAndFacetName,
    sourceObjectReference :: ObjectReference,
    targetObjectReference :: ObjectReference,
    identityAttributeValues :: [AttributeNameAndValue]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedLinkSpecifier' with the minimum fields required to make a request.
--
-- * 'identityAttributeValues' - Identifies the attribute value to update.
-- * 'sourceObjectReference' - Identifies the source object that the typed link will attach to.
-- * 'targetObjectReference' - Identifies the target object that the typed link will attach to.
-- * 'typedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
mkTypedLinkSpecifier ::
  -- | 'typedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  -- | 'sourceObjectReference'
  ObjectReference ->
  -- | 'targetObjectReference'
  ObjectReference ->
  TypedLinkSpecifier
mkTypedLinkSpecifier
  pTypedLinkFacet_
  pSourceObjectReference_
  pTargetObjectReference_ =
    TypedLinkSpecifier'
      { typedLinkFacet = pTypedLinkFacet_,
        sourceObjectReference = pSourceObjectReference_,
        targetObjectReference = pTargetObjectReference_,
        identityAttributeValues = Lude.mempty
      }

-- | Identifies the typed link facet that is associated with the typed link.
--
-- /Note:/ Consider using 'typedLinkFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsTypedLinkFacet :: Lens.Lens' TypedLinkSpecifier TypedLinkSchemaAndFacetName
tlsTypedLinkFacet = Lens.lens (typedLinkFacet :: TypedLinkSpecifier -> TypedLinkSchemaAndFacetName) (\s a -> s {typedLinkFacet = a} :: TypedLinkSpecifier)
{-# DEPRECATED tlsTypedLinkFacet "Use generic-lens or generic-optics with 'typedLinkFacet' instead." #-}

-- | Identifies the source object that the typed link will attach to.
--
-- /Note:/ Consider using 'sourceObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsSourceObjectReference :: Lens.Lens' TypedLinkSpecifier ObjectReference
tlsSourceObjectReference = Lens.lens (sourceObjectReference :: TypedLinkSpecifier -> ObjectReference) (\s a -> s {sourceObjectReference = a} :: TypedLinkSpecifier)
{-# DEPRECATED tlsSourceObjectReference "Use generic-lens or generic-optics with 'sourceObjectReference' instead." #-}

-- | Identifies the target object that the typed link will attach to.
--
-- /Note:/ Consider using 'targetObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsTargetObjectReference :: Lens.Lens' TypedLinkSpecifier ObjectReference
tlsTargetObjectReference = Lens.lens (targetObjectReference :: TypedLinkSpecifier -> ObjectReference) (\s a -> s {targetObjectReference = a} :: TypedLinkSpecifier)
{-# DEPRECATED tlsTargetObjectReference "Use generic-lens or generic-optics with 'targetObjectReference' instead." #-}

-- | Identifies the attribute value to update.
--
-- /Note:/ Consider using 'identityAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlsIdentityAttributeValues :: Lens.Lens' TypedLinkSpecifier [AttributeNameAndValue]
tlsIdentityAttributeValues = Lens.lens (identityAttributeValues :: TypedLinkSpecifier -> [AttributeNameAndValue]) (\s a -> s {identityAttributeValues = a} :: TypedLinkSpecifier)
{-# DEPRECATED tlsIdentityAttributeValues "Use generic-lens or generic-optics with 'identityAttributeValues' instead." #-}

instance Lude.FromJSON TypedLinkSpecifier where
  parseJSON =
    Lude.withObject
      "TypedLinkSpecifier"
      ( \x ->
          TypedLinkSpecifier'
            Lude.<$> (x Lude..: "TypedLinkFacet")
            Lude.<*> (x Lude..: "SourceObjectReference")
            Lude.<*> (x Lude..: "TargetObjectReference")
            Lude.<*> (x Lude..:? "IdentityAttributeValues" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TypedLinkSpecifier where
  toJSON TypedLinkSpecifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TypedLinkFacet" Lude..= typedLinkFacet),
            Lude.Just ("SourceObjectReference" Lude..= sourceObjectReference),
            Lude.Just ("TargetObjectReference" Lude..= targetObjectReference),
            Lude.Just
              ("IdentityAttributeValues" Lude..= identityAttributeValues)
          ]
      )
