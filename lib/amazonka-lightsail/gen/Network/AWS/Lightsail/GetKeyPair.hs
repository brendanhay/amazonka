{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific key pair.
module Network.AWS.Lightsail.GetKeyPair
    (
    -- * Creating a request
      GetKeyPair (..)
    , mkGetKeyPair
    -- ** Request lenses
    , gkpKeyPairName

    -- * Destructuring the response
    , GetKeyPairResponse (..)
    , mkGetKeyPairResponse
    -- ** Response lenses
    , gkprfrsKeyPair
    , gkprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetKeyPair' smart constructor.
newtype GetKeyPair = GetKeyPair'
  { keyPairName :: Types.ResourceName
    -- ^ The name of the key pair for which you are requesting information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyPair' value with any optional fields omitted.
mkGetKeyPair
    :: Types.ResourceName -- ^ 'keyPairName'
    -> GetKeyPair
mkGetKeyPair keyPairName = GetKeyPair'{keyPairName}

-- | The name of the key pair for which you are requesting information.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpKeyPairName :: Lens.Lens' GetKeyPair Types.ResourceName
gkpKeyPairName = Lens.field @"keyPairName"
{-# INLINEABLE gkpKeyPairName #-}
{-# DEPRECATED keyPairName "Use generic-lens or generic-optics with 'keyPairName' instead"  #-}

instance Core.ToQuery GetKeyPair where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetKeyPair where
        toHeaders GetKeyPair{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetKeyPair")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetKeyPair where
        toJSON GetKeyPair{..}
          = Core.object
              (Core.catMaybes [Core.Just ("keyPairName" Core..= keyPairName)])

instance Core.AWSRequest GetKeyPair where
        type Rs GetKeyPair = GetKeyPairResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetKeyPairResponse' Core.<$>
                   (x Core..:? "keyPair") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetKeyPairResponse' smart constructor.
data GetKeyPairResponse = GetKeyPairResponse'
  { keyPair :: Core.Maybe Types.KeyPair
    -- ^ An array of key-value pairs containing information about the key pair.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetKeyPairResponse' value with any optional fields omitted.
mkGetKeyPairResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetKeyPairResponse
mkGetKeyPairResponse responseStatus
  = GetKeyPairResponse'{keyPair = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about the key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprfrsKeyPair :: Lens.Lens' GetKeyPairResponse (Core.Maybe Types.KeyPair)
gkprfrsKeyPair = Lens.field @"keyPair"
{-# INLINEABLE gkprfrsKeyPair #-}
{-# DEPRECATED keyPair "Use generic-lens or generic-optics with 'keyPair' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprfrsResponseStatus :: Lens.Lens' GetKeyPairResponse Core.Int
gkprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gkprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
