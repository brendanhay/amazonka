{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AttachToIndex (..)
    , mkAttachToIndex
    -- ** Request lenses
    , atiDirectoryArn
    , atiIndexReference
    , atiTargetReference

    -- * Destructuring the response
    , AttachToIndexResponse (..)
    , mkAttachToIndexResponse
    -- ** Response lenses
    , atirrsAttachedObjectIdentifier
    , atirrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachToIndex' smart constructor.
data AttachToIndex = AttachToIndex'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the directory where the object and index exist.
  , indexReference :: Types.ObjectReference
    -- ^ A reference to the index that you are attaching the object to.
  , targetReference :: Types.ObjectReference
    -- ^ A reference to the object that you are attaching to the index.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachToIndex' value with any optional fields omitted.
mkAttachToIndex
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'indexReference'
    -> Types.ObjectReference -- ^ 'targetReference'
    -> AttachToIndex
mkAttachToIndex directoryArn indexReference targetReference
  = AttachToIndex'{directoryArn, indexReference, targetReference}

-- | The Amazon Resource Name (ARN) of the directory where the object and index exist.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiDirectoryArn :: Lens.Lens' AttachToIndex Types.Arn
atiDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE atiDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | A reference to the index that you are attaching the object to.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiIndexReference :: Lens.Lens' AttachToIndex Types.ObjectReference
atiIndexReference = Lens.field @"indexReference"
{-# INLINEABLE atiIndexReference #-}
{-# DEPRECATED indexReference "Use generic-lens or generic-optics with 'indexReference' instead"  #-}

-- | A reference to the object that you are attaching to the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiTargetReference :: Lens.Lens' AttachToIndex Types.ObjectReference
atiTargetReference = Lens.field @"targetReference"
{-# INLINEABLE atiTargetReference #-}
{-# DEPRECATED targetReference "Use generic-lens or generic-optics with 'targetReference' instead"  #-}

instance Core.ToQuery AttachToIndex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AttachToIndex where
        toHeaders AttachToIndex{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON AttachToIndex where
        toJSON AttachToIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IndexReference" Core..= indexReference),
                  Core.Just ("TargetReference" Core..= targetReference)])

instance Core.AWSRequest AttachToIndex where
        type Rs AttachToIndex = AttachToIndexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/index/attach",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AttachToIndexResponse' Core.<$>
                   (x Core..:? "AttachedObjectIdentifier") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachToIndexResponse' smart constructor.
data AttachToIndexResponse = AttachToIndexResponse'
  { attachedObjectIdentifier :: Core.Maybe Types.AttachedObjectIdentifier
    -- ^ The @ObjectIdentifier@ of the object that was attached to the index.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachToIndexResponse' value with any optional fields omitted.
mkAttachToIndexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachToIndexResponse
mkAttachToIndexResponse responseStatus
  = AttachToIndexResponse'{attachedObjectIdentifier = Core.Nothing,
                           responseStatus}

-- | The @ObjectIdentifier@ of the object that was attached to the index.
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atirrsAttachedObjectIdentifier :: Lens.Lens' AttachToIndexResponse (Core.Maybe Types.AttachedObjectIdentifier)
atirrsAttachedObjectIdentifier = Lens.field @"attachedObjectIdentifier"
{-# INLINEABLE atirrsAttachedObjectIdentifier #-}
{-# DEPRECATED attachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atirrsResponseStatus :: Lens.Lens' AttachToIndexResponse Core.Int
atirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
