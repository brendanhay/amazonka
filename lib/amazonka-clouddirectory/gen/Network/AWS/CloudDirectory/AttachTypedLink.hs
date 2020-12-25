{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AttachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.AttachTypedLink
  ( -- * Creating a request
    AttachTypedLink (..),
    mkAttachTypedLink,

    -- ** Request lenses
    atlDirectoryArn,
    atlSourceObjectReference,
    atlTargetObjectReference,
    atlTypedLinkFacet,
    atlAttributes,

    -- * Destructuring the response
    AttachTypedLinkResponse (..),
    mkAttachTypedLinkResponse,

    -- ** Response lenses
    atlrrsTypedLinkSpecifier,
    atlrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachTypedLink' smart constructor.
data AttachTypedLink = AttachTypedLink'
  { -- | The Amazon Resource Name (ARN) of the directory where you want to attach the typed link.
    directoryArn :: Types.Arn,
    -- | Identifies the source object that the typed link will attach to.
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

-- | Creates a 'AttachTypedLink' value with any optional fields omitted.
mkAttachTypedLink ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'sourceObjectReference'
  Types.ObjectReference ->
  -- | 'targetObjectReference'
  Types.ObjectReference ->
  -- | 'typedLinkFacet'
  Types.TypedLinkSchemaAndFacetName ->
  AttachTypedLink
mkAttachTypedLink
  directoryArn
  sourceObjectReference
  targetObjectReference
  typedLinkFacet =
    AttachTypedLink'
      { directoryArn,
        sourceObjectReference,
        targetObjectReference,
        typedLinkFacet,
        attributes = Core.mempty
      }

-- | The Amazon Resource Name (ARN) of the directory where you want to attach the typed link.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlDirectoryArn :: Lens.Lens' AttachTypedLink Types.Arn
atlDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED atlDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | Identifies the source object that the typed link will attach to.
--
-- /Note:/ Consider using 'sourceObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlSourceObjectReference :: Lens.Lens' AttachTypedLink Types.ObjectReference
atlSourceObjectReference = Lens.field @"sourceObjectReference"
{-# DEPRECATED atlSourceObjectReference "Use generic-lens or generic-optics with 'sourceObjectReference' instead." #-}

-- | Identifies the target object that the typed link will attach to.
--
-- /Note:/ Consider using 'targetObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlTargetObjectReference :: Lens.Lens' AttachTypedLink Types.ObjectReference
atlTargetObjectReference = Lens.field @"targetObjectReference"
{-# DEPRECATED atlTargetObjectReference "Use generic-lens or generic-optics with 'targetObjectReference' instead." #-}

-- | Identifies the typed link facet that is associated with the typed link.
--
-- /Note:/ Consider using 'typedLinkFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlTypedLinkFacet :: Lens.Lens' AttachTypedLink Types.TypedLinkSchemaAndFacetName
atlTypedLinkFacet = Lens.field @"typedLinkFacet"
{-# DEPRECATED atlTypedLinkFacet "Use generic-lens or generic-optics with 'typedLinkFacet' instead." #-}

-- | A set of attributes that are associated with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlAttributes :: Lens.Lens' AttachTypedLink [Types.AttributeNameAndValue]
atlAttributes = Lens.field @"attributes"
{-# DEPRECATED atlAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.FromJSON AttachTypedLink where
  toJSON AttachTypedLink {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceObjectReference" Core..= sourceObjectReference),
            Core.Just ("TargetObjectReference" Core..= targetObjectReference),
            Core.Just ("TypedLinkFacet" Core..= typedLinkFacet),
            Core.Just ("Attributes" Core..= attributes)
          ]
      )

instance Core.AWSRequest AttachTypedLink where
  type Rs AttachTypedLink = AttachTypedLinkResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/typedlink/attach",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachTypedLinkResponse'
            Core.<$> (x Core..:? "TypedLinkSpecifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAttachTypedLinkResponse' smart constructor.
data AttachTypedLinkResponse = AttachTypedLinkResponse'
  { -- | Returns a typed link specifier as output.
    typedLinkSpecifier :: Core.Maybe Types.TypedLinkSpecifier,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AttachTypedLinkResponse' value with any optional fields omitted.
mkAttachTypedLinkResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachTypedLinkResponse
mkAttachTypedLinkResponse responseStatus =
  AttachTypedLinkResponse'
    { typedLinkSpecifier = Core.Nothing,
      responseStatus
    }

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlrrsTypedLinkSpecifier :: Lens.Lens' AttachTypedLinkResponse (Core.Maybe Types.TypedLinkSpecifier)
atlrrsTypedLinkSpecifier = Lens.field @"typedLinkSpecifier"
{-# DEPRECATED atlrrsTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlrrsResponseStatus :: Lens.Lens' AttachTypedLinkResponse Core.Int
atlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
