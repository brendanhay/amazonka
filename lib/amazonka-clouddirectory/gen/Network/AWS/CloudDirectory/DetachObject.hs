{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a given object from the parent object. The object that is to be detached from the parent is specified by the link name.
module Network.AWS.CloudDirectory.DetachObject
    (
    -- * Creating a request
      DetachObject (..)
    , mkDetachObject
    -- ** Request lenses
    , dofDirectoryArn
    , dofParentReference
    , dofLinkName

    -- * Destructuring the response
    , DetachObjectResponse (..)
    , mkDetachObjectResponse
    -- ** Response lenses
    , dorfrsDetachedObjectIdentifier
    , dorfrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachObject' smart constructor.
data DetachObject = DetachObject'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
  , parentReference :: Types.ObjectReference
    -- ^ The parent reference from which the object with the specified link name is detached.
  , linkName :: Types.LinkName
    -- ^ The link name associated with the object that needs to be detached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachObject' value with any optional fields omitted.
mkDetachObject
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'parentReference'
    -> Types.LinkName -- ^ 'linkName'
    -> DetachObject
mkDetachObject directoryArn parentReference linkName
  = DetachObject'{directoryArn, parentReference, linkName}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofDirectoryArn :: Lens.Lens' DetachObject Types.Arn
dofDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE dofDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The parent reference from which the object with the specified link name is detached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofParentReference :: Lens.Lens' DetachObject Types.ObjectReference
dofParentReference = Lens.field @"parentReference"
{-# INLINEABLE dofParentReference #-}
{-# DEPRECATED parentReference "Use generic-lens or generic-optics with 'parentReference' instead"  #-}

-- | The link name associated with the object that needs to be detached.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofLinkName :: Lens.Lens' DetachObject Types.LinkName
dofLinkName = Lens.field @"linkName"
{-# INLINEABLE dofLinkName #-}
{-# DEPRECATED linkName "Use generic-lens or generic-optics with 'linkName' instead"  #-}

instance Core.ToQuery DetachObject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetachObject where
        toHeaders DetachObject{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON DetachObject where
        toJSON DetachObject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ParentReference" Core..= parentReference),
                  Core.Just ("LinkName" Core..= linkName)])

instance Core.AWSRequest DetachObject where
        type Rs DetachObject = DetachObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/object/detach",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetachObjectResponse' Core.<$>
                   (x Core..:? "DetachedObjectIdentifier") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachObjectResponse' smart constructor.
data DetachObjectResponse = DetachObjectResponse'
  { detachedObjectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The @ObjectIdentifier@ that was detached from the object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachObjectResponse' value with any optional fields omitted.
mkDetachObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachObjectResponse
mkDetachObjectResponse responseStatus
  = DetachObjectResponse'{detachedObjectIdentifier = Core.Nothing,
                          responseStatus}

-- | The @ObjectIdentifier@ that was detached from the object.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorfrsDetachedObjectIdentifier :: Lens.Lens' DetachObjectResponse (Core.Maybe Types.ObjectIdentifier)
dorfrsDetachedObjectIdentifier = Lens.field @"detachedObjectIdentifier"
{-# INLINEABLE dorfrsDetachedObjectIdentifier #-}
{-# DEPRECATED detachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorfrsResponseStatus :: Lens.Lens' DetachObjectResponse Core.Int
dorfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
