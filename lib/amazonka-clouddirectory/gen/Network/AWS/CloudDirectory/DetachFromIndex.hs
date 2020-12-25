{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachFromIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified object from the specified index.
module Network.AWS.CloudDirectory.DetachFromIndex
  ( -- * Creating a request
    DetachFromIndex (..),
    mkDetachFromIndex,

    -- ** Request lenses
    dfiDirectoryArn,
    dfiIndexReference,
    dfiTargetReference,

    -- * Destructuring the response
    DetachFromIndexResponse (..),
    mkDetachFromIndexResponse,

    -- ** Response lenses
    dfirrsDetachedObjectIdentifier,
    dfirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachFromIndex' smart constructor.
data DetachFromIndex = DetachFromIndex'
  { -- | The Amazon Resource Name (ARN) of the directory the index and object exist in.
    directoryArn :: Types.Arn,
    -- | A reference to the index object.
    indexReference :: Types.ObjectReference,
    -- | A reference to the object being detached from the index.
    targetReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachFromIndex' value with any optional fields omitted.
mkDetachFromIndex ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'indexReference'
  Types.ObjectReference ->
  -- | 'targetReference'
  Types.ObjectReference ->
  DetachFromIndex
mkDetachFromIndex directoryArn indexReference targetReference =
  DetachFromIndex' {directoryArn, indexReference, targetReference}

-- | The Amazon Resource Name (ARN) of the directory the index and object exist in.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiDirectoryArn :: Lens.Lens' DetachFromIndex Types.Arn
dfiDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED dfiDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | A reference to the index object.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiIndexReference :: Lens.Lens' DetachFromIndex Types.ObjectReference
dfiIndexReference = Lens.field @"indexReference"
{-# DEPRECATED dfiIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

-- | A reference to the object being detached from the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiTargetReference :: Lens.Lens' DetachFromIndex Types.ObjectReference
dfiTargetReference = Lens.field @"targetReference"
{-# DEPRECATED dfiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

instance Core.FromJSON DetachFromIndex where
  toJSON DetachFromIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IndexReference" Core..= indexReference),
            Core.Just ("TargetReference" Core..= targetReference)
          ]
      )

instance Core.AWSRequest DetachFromIndex where
  type Rs DetachFromIndex = DetachFromIndexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/index/detach",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachFromIndexResponse'
            Core.<$> (x Core..:? "DetachedObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetachFromIndexResponse' smart constructor.
data DetachFromIndexResponse = DetachFromIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was detached from the index.
    detachedObjectIdentifier :: Core.Maybe Types.DetachedObjectIdentifier,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachFromIndexResponse' value with any optional fields omitted.
mkDetachFromIndexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachFromIndexResponse
mkDetachFromIndexResponse responseStatus =
  DetachFromIndexResponse'
    { detachedObjectIdentifier = Core.Nothing,
      responseStatus
    }

-- | The @ObjectIdentifier@ of the object that was detached from the index.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirrsDetachedObjectIdentifier :: Lens.Lens' DetachFromIndexResponse (Core.Maybe Types.DetachedObjectIdentifier)
dfirrsDetachedObjectIdentifier = Lens.field @"detachedObjectIdentifier"
{-# DEPRECATED dfirrsDetachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirrsResponseStatus :: Lens.Lens' DetachFromIndexResponse Core.Int
dfirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
