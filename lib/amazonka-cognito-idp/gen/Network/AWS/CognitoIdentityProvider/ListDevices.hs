{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the devices.
module Network.AWS.CognitoIdentityProvider.ListDevices
    (
    -- * Creating a request
      ListDevices (..)
    , mkListDevices
    -- ** Request lenses
    , ldAccessToken
    , ldLimit
    , ldPaginationToken

    -- * Destructuring the response
    , ListDevicesResponse (..)
    , mkListDevicesResponse
    -- ** Response lenses
    , ldrrsDevices
    , ldrrsPaginationToken
    , ldrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the devices.
--
-- /See:/ 'mkListDevices' smart constructor.
data ListDevices = ListDevices'
  { accessToken :: Types.TokenModelType
    -- ^ The access tokens for the request to list devices.
  , limit :: Core.Maybe Core.Natural
    -- ^ The limit of the device request.
  , paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ The pagination token for the list request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDevices' value with any optional fields omitted.
mkListDevices
    :: Types.TokenModelType -- ^ 'accessToken'
    -> ListDevices
mkListDevices accessToken
  = ListDevices'{accessToken, limit = Core.Nothing,
                 paginationToken = Core.Nothing}

-- | The access tokens for the request to list devices.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAccessToken :: Lens.Lens' ListDevices Types.TokenModelType
ldAccessToken = Lens.field @"accessToken"
{-# INLINEABLE ldAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

-- | The limit of the device request.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLimit :: Lens.Lens' ListDevices (Core.Maybe Core.Natural)
ldLimit = Lens.field @"limit"
{-# INLINEABLE ldLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The pagination token for the list request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldPaginationToken :: Lens.Lens' ListDevices (Core.Maybe Types.PaginationToken)
ldPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE ldPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

instance Core.ToQuery ListDevices where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDevices where
        toHeaders ListDevices{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.ListDevices")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDevices where
        toJSON ListDevices{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccessToken" Core..= accessToken),
                  ("Limit" Core..=) Core.<$> limit,
                  ("PaginationToken" Core..=) Core.<$> paginationToken])

instance Core.AWSRequest ListDevices where
        type Rs ListDevices = ListDevicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDevicesResponse' Core.<$>
                   (x Core..:? "Devices") Core.<*> x Core..:? "PaginationToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response to list devices.
--
-- /See:/ 'mkListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { devices :: Core.Maybe [Types.DeviceType]
    -- ^ The devices returned in the list devices response.
  , paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ The pagination token for the list device response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDevicesResponse' value with any optional fields omitted.
mkListDevicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDevicesResponse
mkListDevicesResponse responseStatus
  = ListDevicesResponse'{devices = Core.Nothing,
                         paginationToken = Core.Nothing, responseStatus}

-- | The devices returned in the list devices response.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDevices :: Lens.Lens' ListDevicesResponse (Core.Maybe [Types.DeviceType])
ldrrsDevices = Lens.field @"devices"
{-# INLINEABLE ldrrsDevices #-}
{-# DEPRECATED devices "Use generic-lens or generic-optics with 'devices' instead"  #-}

-- | The pagination token for the list device response.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsPaginationToken :: Lens.Lens' ListDevicesResponse (Core.Maybe Types.PaginationToken)
ldrrsPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE ldrrsPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDevicesResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
