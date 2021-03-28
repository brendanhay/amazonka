{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetContactAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contact attributes for the specified contact.
module Network.AWS.Connect.GetContactAttributes
    (
    -- * Creating a request
      GetContactAttributes (..)
    , mkGetContactAttributes
    -- ** Request lenses
    , gcaInstanceId
    , gcaInitialContactId

    -- * Destructuring the response
    , GetContactAttributesResponse (..)
    , mkGetContactAttributesResponse
    -- ** Response lenses
    , gcarrsAttributes
    , gcarrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContactAttributes' smart constructor.
data GetContactAttributes = GetContactAttributes'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , initialContactId :: Types.InitialContactId
    -- ^ The identifier of the initial contact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContactAttributes' value with any optional fields omitted.
mkGetContactAttributes
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.InitialContactId -- ^ 'initialContactId'
    -> GetContactAttributes
mkGetContactAttributes instanceId initialContactId
  = GetContactAttributes'{instanceId, initialContactId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaInstanceId :: Lens.Lens' GetContactAttributes Types.InstanceId
gcaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gcaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the initial contact.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaInitialContactId :: Lens.Lens' GetContactAttributes Types.InitialContactId
gcaInitialContactId = Lens.field @"initialContactId"
{-# INLINEABLE gcaInitialContactId #-}
{-# DEPRECATED initialContactId "Use generic-lens or generic-optics with 'initialContactId' instead"  #-}

instance Core.ToQuery GetContactAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetContactAttributes where
        toHeaders GetContactAttributes{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetContactAttributes where
        type Rs GetContactAttributes = GetContactAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/contact/attributes/" Core.<> Core.toText instanceId Core.<> "/"
                             Core.<> Core.toText initialContactId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetContactAttributesResponse' Core.<$>
                   (x Core..:? "Attributes") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetContactAttributesResponse' smart constructor.
data GetContactAttributesResponse = GetContactAttributesResponse'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ Information about the attributes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContactAttributesResponse' value with any optional fields omitted.
mkGetContactAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetContactAttributesResponse
mkGetContactAttributesResponse responseStatus
  = GetContactAttributesResponse'{attributes = Core.Nothing,
                                  responseStatus}

-- | Information about the attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcarrsAttributes :: Lens.Lens' GetContactAttributesResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
gcarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE gcarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcarrsResponseStatus :: Lens.Lens' GetContactAttributesResponse Core.Int
gcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
