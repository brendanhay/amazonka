{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachTypedLink
  ( BatchAttachTypedLink (..),

    -- * Smart constructor
    mkBatchAttachTypedLink,

    -- * Lenses
    batlTargetObjectReference,
    batlSourceObjectReference,
    batlAttributes,
    batlTypedLinkFacet,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attaches a typed link to a specified source and target object inside a 'BatchRead' operation. For more information, see 'AttachTypedLink' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchAttachTypedLink' smart constructor.
data BatchAttachTypedLink = BatchAttachTypedLink'
  { -- | Identifies the target object that the typed link will attach to.
    targetObjectReference :: ObjectReference,
    -- | Identifies the source object that the typed link will attach to.
    sourceObjectReference :: ObjectReference,
    -- | A set of attributes that are associated with the typed link.
    attributes :: [AttributeNameAndValue],
    -- | Identifies the typed link facet that is associated with the typed link.
    typedLinkFacet :: TypedLinkSchemaAndFacetName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAttachTypedLink' with the minimum fields required to make a request.
--
-- * 'targetObjectReference' - Identifies the target object that the typed link will attach to.
-- * 'sourceObjectReference' - Identifies the source object that the typed link will attach to.
-- * 'attributes' - A set of attributes that are associated with the typed link.
-- * 'typedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
mkBatchAttachTypedLink ::
  -- | 'targetObjectReference'
  ObjectReference ->
  -- | 'sourceObjectReference'
  ObjectReference ->
  -- | 'typedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  BatchAttachTypedLink
mkBatchAttachTypedLink
  pTargetObjectReference_
  pSourceObjectReference_
  pTypedLinkFacet_ =
    BatchAttachTypedLink'
      { targetObjectReference =
          pTargetObjectReference_,
        sourceObjectReference = pSourceObjectReference_,
        attributes = Lude.mempty,
        typedLinkFacet = pTypedLinkFacet_
      }

-- | Identifies the target object that the typed link will attach to.
--
-- /Note:/ Consider using 'targetObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlTargetObjectReference :: Lens.Lens' BatchAttachTypedLink ObjectReference
batlTargetObjectReference = Lens.lens (targetObjectReference :: BatchAttachTypedLink -> ObjectReference) (\s a -> s {targetObjectReference = a} :: BatchAttachTypedLink)
{-# DEPRECATED batlTargetObjectReference "Use generic-lens or generic-optics with 'targetObjectReference' instead." #-}

-- | Identifies the source object that the typed link will attach to.
--
-- /Note:/ Consider using 'sourceObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlSourceObjectReference :: Lens.Lens' BatchAttachTypedLink ObjectReference
batlSourceObjectReference = Lens.lens (sourceObjectReference :: BatchAttachTypedLink -> ObjectReference) (\s a -> s {sourceObjectReference = a} :: BatchAttachTypedLink)
{-# DEPRECATED batlSourceObjectReference "Use generic-lens or generic-optics with 'sourceObjectReference' instead." #-}

-- | A set of attributes that are associated with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlAttributes :: Lens.Lens' BatchAttachTypedLink [AttributeNameAndValue]
batlAttributes = Lens.lens (attributes :: BatchAttachTypedLink -> [AttributeNameAndValue]) (\s a -> s {attributes = a} :: BatchAttachTypedLink)
{-# DEPRECATED batlAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Identifies the typed link facet that is associated with the typed link.
--
-- /Note:/ Consider using 'typedLinkFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlTypedLinkFacet :: Lens.Lens' BatchAttachTypedLink TypedLinkSchemaAndFacetName
batlTypedLinkFacet = Lens.lens (typedLinkFacet :: BatchAttachTypedLink -> TypedLinkSchemaAndFacetName) (\s a -> s {typedLinkFacet = a} :: BatchAttachTypedLink)
{-# DEPRECATED batlTypedLinkFacet "Use generic-lens or generic-optics with 'typedLinkFacet' instead." #-}

instance Lude.ToJSON BatchAttachTypedLink where
  toJSON BatchAttachTypedLink' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetObjectReference" Lude..= targetObjectReference),
            Lude.Just ("SourceObjectReference" Lude..= sourceObjectReference),
            Lude.Just ("Attributes" Lude..= attributes),
            Lude.Just ("TypedLinkFacet" Lude..= typedLinkFacet)
          ]
      )
