{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AttachToIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified object to the specified index.
module Network.AWS.CloudDirectory.AttachToIndex
  ( -- * Creating a request
    AttachToIndex (..),
    mkAttachToIndex,

    -- ** Request lenses
    atiDirectoryArn,
    atiIndexReference,
    atiTargetReference,

    -- * Destructuring the response
    AttachToIndexResponse (..),
    mkAttachToIndexResponse,

    -- ** Response lenses
    atirrsAttachedObjectIdentifier,
    atirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachToIndex' smart constructor.
data AttachToIndex = AttachToIndex'
  { -- | The Amazon Resource Name (ARN) of the directory where the object and index exist.
    directoryArn :: Types.Arn,
    -- | A reference to the index that you are attaching the object to.
    indexReference :: Types.ObjectReference,
    -- | A reference to the object that you are attaching to the index.
    targetReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachToIndex' value with any optional fields omitted.
mkAttachToIndex ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'indexReference'
  Types.ObjectReference ->
  -- | 'targetReference'
  Types.ObjectReference ->
  AttachToIndex
mkAttachToIndex directoryArn indexReference targetReference =
  AttachToIndex' {directoryArn, indexReference, targetReference}

-- | The Amazon Resource Name (ARN) of the directory where the object and index exist.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiDirectoryArn :: Lens.Lens' AttachToIndex Types.Arn
atiDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED atiDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | A reference to the index that you are attaching the object to.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiIndexReference :: Lens.Lens' AttachToIndex Types.ObjectReference
atiIndexReference = Lens.field @"indexReference"
{-# DEPRECATED atiIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

-- | A reference to the object that you are attaching to the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiTargetReference :: Lens.Lens' AttachToIndex Types.ObjectReference
atiTargetReference = Lens.field @"targetReference"
{-# DEPRECATED atiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

instance Core.FromJSON AttachToIndex where
  toJSON AttachToIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IndexReference" Core..= indexReference),
            Core.Just ("TargetReference" Core..= targetReference)
          ]
      )

instance Core.AWSRequest AttachToIndex where
  type Rs AttachToIndex = AttachToIndexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/index/attach",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachToIndexResponse'
            Core.<$> (x Core..:? "AttachedObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAttachToIndexResponse' smart constructor.
data AttachToIndexResponse = AttachToIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was attached to the index.
    attachedObjectIdentifier :: Core.Maybe Types.AttachedObjectIdentifier,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachToIndexResponse' value with any optional fields omitted.
mkAttachToIndexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachToIndexResponse
mkAttachToIndexResponse responseStatus =
  AttachToIndexResponse'
    { attachedObjectIdentifier = Core.Nothing,
      responseStatus
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atirrsAttachedObjectIdentifier :: Lens.Lens' AttachToIndexResponse (Core.Maybe Types.AttachedObjectIdentifier)
atirrsAttachedObjectIdentifier = Lens.field @"attachedObjectIdentifier"
{-# DEPRECATED atirrsAttachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atirrsResponseStatus :: Lens.Lens' AttachToIndexResponse Core.Int
atirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
