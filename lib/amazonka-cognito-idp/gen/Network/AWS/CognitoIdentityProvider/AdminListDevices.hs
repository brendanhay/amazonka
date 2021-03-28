{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists devices, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminListDevices
    (
    -- * Creating a request
      AdminListDevices (..)
    , mkAdminListDevices
    -- ** Request lenses
    , aldUserPoolId
    , aldUsername
    , aldLimit
    , aldPaginationToken

    -- * Destructuring the response
    , AdminListDevicesResponse (..)
    , mkAdminListDevicesResponse
    -- ** Response lenses
    , aldrrsDevices
    , aldrrsPaginationToken
    , aldrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list devices, as an administrator.
--
-- /See:/ 'mkAdminListDevices' smart constructor.
data AdminListDevices = AdminListDevices'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , username :: Types.Username
    -- ^ The user name.
  , limit :: Core.Maybe Core.Natural
    -- ^ The limit of the devices request.
  , paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminListDevices' value with any optional fields omitted.
mkAdminListDevices
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> AdminListDevices
mkAdminListDevices userPoolId username
  = AdminListDevices'{userPoolId, username, limit = Core.Nothing,
                      paginationToken = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldUserPoolId :: Lens.Lens' AdminListDevices Types.UserPoolId
aldUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE aldUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldUsername :: Lens.Lens' AdminListDevices Types.Username
aldUsername = Lens.field @"username"
{-# INLINEABLE aldUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The limit of the devices request.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldLimit :: Lens.Lens' AdminListDevices (Core.Maybe Core.Natural)
aldLimit = Lens.field @"limit"
{-# INLINEABLE aldLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldPaginationToken :: Lens.Lens' AdminListDevices (Core.Maybe Types.PaginationToken)
aldPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE aldPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

instance Core.ToQuery AdminListDevices where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminListDevices where
        toHeaders AdminListDevices{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminListDevices")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminListDevices where
        toJSON AdminListDevices{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  ("Limit" Core..=) Core.<$> limit,
                  ("PaginationToken" Core..=) Core.<$> paginationToken])

instance Core.AWSRequest AdminListDevices where
        type Rs AdminListDevices = AdminListDevicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AdminListDevicesResponse' Core.<$>
                   (x Core..:? "Devices") Core.<*> x Core..:? "PaginationToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Lists the device's response, as an administrator.
--
-- /See:/ 'mkAdminListDevicesResponse' smart constructor.
data AdminListDevicesResponse = AdminListDevicesResponse'
  { devices :: Core.Maybe [Types.DeviceType]
    -- ^ The devices in the list of devices response.
  , paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ The pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AdminListDevicesResponse' value with any optional fields omitted.
mkAdminListDevicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminListDevicesResponse
mkAdminListDevicesResponse responseStatus
  = AdminListDevicesResponse'{devices = Core.Nothing,
                              paginationToken = Core.Nothing, responseStatus}

-- | The devices in the list of devices response.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldrrsDevices :: Lens.Lens' AdminListDevicesResponse (Core.Maybe [Types.DeviceType])
aldrrsDevices = Lens.field @"devices"
{-# INLINEABLE aldrrsDevices #-}
{-# DEPRECATED devices "Use generic-lens or generic-optics with 'devices' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldrrsPaginationToken :: Lens.Lens' AdminListDevicesResponse (Core.Maybe Types.PaginationToken)
aldrrsPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE aldrrsPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldrrsResponseStatus :: Lens.Lens' AdminListDevicesResponse Core.Int
aldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
