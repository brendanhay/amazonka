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
    batlSourceObjectReference,
    batlTargetObjectReference,
    batlTypedLinkFacet,
    batlAttributes,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeNameAndValue as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attaches a typed link to a specified source and target object inside a 'BatchRead' operation. For more information, see 'AttachTypedLink' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchAttachTypedLink' smart constructor.
data BatchAttachTypedLink = BatchAttachTypedLink'
  { -- | Identifies the source object that the typed link will attach to.
    sourceObjectReference :: Types.ObjectReference,
    -- | Identifies the target object that the typed link will attach to.
    targetObjectReference :: Types.ObjectReference,
    -- | Identifies the typed link facet that is associated with the typed link.
    typedLinkFacet :: Types.TypedLinkSchemaAndFacetName,
    -- | A set of attributes that are associated with the typed link.
    attributes :: [Types.AttributeNameAndValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchAttachTypedLink' value with any optional fields omitted.
mkBatchAttachTypedLink ::
  -- | 'sourceObjectReference'
  Types.ObjectReference ->
  -- | 'targetObjectReference'
  Types.ObjectReference ->
  -- | 'typedLinkFacet'
  Types.TypedLinkSchemaAndFacetName ->
  BatchAttachTypedLink
mkBatchAttachTypedLink
  sourceObjectReference
  targetObjectReference
  typedLinkFacet =
    BatchAttachTypedLink'
      { sourceObjectReference,
        targetObjectReference,
        typedLinkFacet,
        attributes = Core.mempty
      }

-- | Identifies the source object that the typed link will attach to.
--
-- /Note:/ Consider using 'sourceObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlSourceObjectReference :: Lens.Lens' BatchAttachTypedLink Types.ObjectReference
batlSourceObjectReference = Lens.field @"sourceObjectReference"
{-# DEPRECATED batlSourceObjectReference "Use generic-lens or generic-optics with 'sourceObjectReference' instead." #-}

-- | Identifies the target object that the typed link will attach to.
--
-- /Note:/ Consider using 'targetObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlTargetObjectReference :: Lens.Lens' BatchAttachTypedLink Types.ObjectReference
batlTargetObjectReference = Lens.field @"targetObjectReference"
{-# DEPRECATED batlTargetObjectReference "Use generic-lens or generic-optics with 'targetObjectReference' instead." #-}

-- | Identifies the typed link facet that is associated with the typed link.
--
-- /Note:/ Consider using 'typedLinkFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlTypedLinkFacet :: Lens.Lens' BatchAttachTypedLink Types.TypedLinkSchemaAndFacetName
batlTypedLinkFacet = Lens.field @"typedLinkFacet"
{-# DEPRECATED batlTypedLinkFacet "Use generic-lens or generic-optics with 'typedLinkFacet' instead." #-}

-- | A set of attributes that are associated with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlAttributes :: Lens.Lens' BatchAttachTypedLink [Types.AttributeNameAndValue]
batlAttributes = Lens.field @"attributes"
{-# DEPRECATED batlAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.FromJSON BatchAttachTypedLink where
  toJSON BatchAttachTypedLink {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceObjectReference" Core..= sourceObjectReference),
            Core.Just ("TargetObjectReference" Core..= targetObjectReference),
            Core.Just ("TypedLinkFacet" Core..= typedLinkFacet),
            Core.Just ("Attributes" Core..= attributes)
          ]
      )
