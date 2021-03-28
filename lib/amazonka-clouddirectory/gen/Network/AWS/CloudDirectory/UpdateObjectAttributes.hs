{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given object's attributes.
module Network.AWS.CloudDirectory.UpdateObjectAttributes
    (
    -- * Creating a request
      UpdateObjectAttributes (..)
    , mkUpdateObjectAttributes
    -- ** Request lenses
    , uoaDirectoryArn
    , uoaObjectReference
    , uoaAttributeUpdates

    -- * Destructuring the response
    , UpdateObjectAttributesResponse (..)
    , mkUpdateObjectAttributesResponse
    -- ** Response lenses
    , uoarrsObjectIdentifier
    , uoarrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateObjectAttributes' smart constructor.
data UpdateObjectAttributes = UpdateObjectAttributes'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
  , objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object.
  , attributeUpdates :: [Types.ObjectAttributeUpdate]
    -- ^ The attributes update structure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateObjectAttributes' value with any optional fields omitted.
mkUpdateObjectAttributes
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> UpdateObjectAttributes
mkUpdateObjectAttributes directoryArn objectReference
  = UpdateObjectAttributes'{directoryArn, objectReference,
                            attributeUpdates = Core.mempty}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoaDirectoryArn :: Lens.Lens' UpdateObjectAttributes Types.Arn
uoaDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE uoaDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The reference that identifies the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoaObjectReference :: Lens.Lens' UpdateObjectAttributes Types.ObjectReference
uoaObjectReference = Lens.field @"objectReference"
{-# INLINEABLE uoaObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | The attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoaAttributeUpdates :: Lens.Lens' UpdateObjectAttributes [Types.ObjectAttributeUpdate]
uoaAttributeUpdates = Lens.field @"attributeUpdates"
{-# INLINEABLE uoaAttributeUpdates #-}
{-# DEPRECATED attributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead"  #-}

instance Core.ToQuery UpdateObjectAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateObjectAttributes where
        toHeaders UpdateObjectAttributes{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON UpdateObjectAttributes where
        toJSON UpdateObjectAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  Core.Just ("AttributeUpdates" Core..= attributeUpdates)])

instance Core.AWSRequest UpdateObjectAttributes where
        type Rs UpdateObjectAttributes = UpdateObjectAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/object/update",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateObjectAttributesResponse' Core.<$>
                   (x Core..:? "ObjectIdentifier") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateObjectAttributesResponse' smart constructor.
data UpdateObjectAttributesResponse = UpdateObjectAttributesResponse'
  { objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The @ObjectIdentifier@ of the updated object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateObjectAttributesResponse' value with any optional fields omitted.
mkUpdateObjectAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateObjectAttributesResponse
mkUpdateObjectAttributesResponse responseStatus
  = UpdateObjectAttributesResponse'{objectIdentifier = Core.Nothing,
                                    responseStatus}

-- | The @ObjectIdentifier@ of the updated object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoarrsObjectIdentifier :: Lens.Lens' UpdateObjectAttributesResponse (Core.Maybe Types.ObjectIdentifier)
uoarrsObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE uoarrsObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoarrsResponseStatus :: Lens.Lens' UpdateObjectAttributesResponse Core.Int
uoarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uoarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
