{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateSecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a security key to the instance.
module Network.AWS.Connect.AssociateSecurityKey
    (
    -- * Creating a request
      AssociateSecurityKey (..)
    , mkAssociateSecurityKey
    -- ** Request lenses
    , askInstanceId
    , askKey

    -- * Destructuring the response
    , AssociateSecurityKeyResponse (..)
    , mkAssociateSecurityKeyResponse
    -- ** Response lenses
    , askrrsAssociationId
    , askrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateSecurityKey' smart constructor.
data AssociateSecurityKey = AssociateSecurityKey'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , key :: Types.PEM
    -- ^ A valid security key in PEM format.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSecurityKey' value with any optional fields omitted.
mkAssociateSecurityKey
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.PEM -- ^ 'key'
    -> AssociateSecurityKey
mkAssociateSecurityKey instanceId key
  = AssociateSecurityKey'{instanceId, key}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askInstanceId :: Lens.Lens' AssociateSecurityKey Types.InstanceId
askInstanceId = Lens.field @"instanceId"
{-# INLINEABLE askInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | A valid security key in PEM format.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askKey :: Lens.Lens' AssociateSecurityKey Types.PEM
askKey = Lens.field @"key"
{-# INLINEABLE askKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

instance Core.ToQuery AssociateSecurityKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateSecurityKey where
        toHeaders AssociateSecurityKey{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateSecurityKey where
        toJSON AssociateSecurityKey{..}
          = Core.object (Core.catMaybes [Core.Just ("Key" Core..= key)])

instance Core.AWSRequest AssociateSecurityKey where
        type Rs AssociateSecurityKey = AssociateSecurityKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<>
                             "/security-key",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssociateSecurityKeyResponse' Core.<$>
                   (x Core..:? "AssociationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateSecurityKeyResponse' smart constructor.
data AssociateSecurityKeyResponse = AssociateSecurityKeyResponse'
  { associationId :: Core.Maybe Types.AssociationId
    -- ^ The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSecurityKeyResponse' value with any optional fields omitted.
mkAssociateSecurityKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateSecurityKeyResponse
mkAssociateSecurityKeyResponse responseStatus
  = AssociateSecurityKeyResponse'{associationId = Core.Nothing,
                                  responseStatus}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askrrsAssociationId :: Lens.Lens' AssociateSecurityKeyResponse (Core.Maybe Types.AssociationId)
askrrsAssociationId = Lens.field @"associationId"
{-# INLINEABLE askrrsAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
askrrsResponseStatus :: Lens.Lens' AssociateSecurityKeyResponse Core.Int
askrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE askrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
