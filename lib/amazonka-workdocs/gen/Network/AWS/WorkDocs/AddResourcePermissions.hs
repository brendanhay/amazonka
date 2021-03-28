{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.AddResourcePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of permissions for the specified folder or document. The resource permissions are overwritten if the principals already have different permissions.
module Network.AWS.WorkDocs.AddResourcePermissions
    (
    -- * Creating a request
      AddResourcePermissions (..)
    , mkAddResourcePermissions
    -- ** Request lenses
    , arpResourceId
    , arpPrincipals
    , arpAuthenticationToken
    , arpNotificationOptions

    -- * Destructuring the response
    , AddResourcePermissionsResponse (..)
    , mkAddResourcePermissionsResponse
    -- ** Response lenses
    , arprrsShareResults
    , arprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkAddResourcePermissions' smart constructor.
data AddResourcePermissions = AddResourcePermissions'
  { resourceId :: Types.ResourceIdType
    -- ^ The ID of the resource.
  , principals :: [Types.SharePrincipal]
    -- ^ The users, groups, or organization being granted permission.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , notificationOptions :: Core.Maybe Types.NotificationOptions
    -- ^ The notification options.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddResourcePermissions' value with any optional fields omitted.
mkAddResourcePermissions
    :: Types.ResourceIdType -- ^ 'resourceId'
    -> AddResourcePermissions
mkAddResourcePermissions resourceId
  = AddResourcePermissions'{resourceId, principals = Core.mempty,
                            authenticationToken = Core.Nothing,
                            notificationOptions = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpResourceId :: Lens.Lens' AddResourcePermissions Types.ResourceIdType
arpResourceId = Lens.field @"resourceId"
{-# INLINEABLE arpResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The users, groups, or organization being granted permission.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpPrincipals :: Lens.Lens' AddResourcePermissions [Types.SharePrincipal]
arpPrincipals = Lens.field @"principals"
{-# INLINEABLE arpPrincipals #-}
{-# DEPRECATED principals "Use generic-lens or generic-optics with 'principals' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpAuthenticationToken :: Lens.Lens' AddResourcePermissions (Core.Maybe Types.AuthenticationHeaderType)
arpAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE arpAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The notification options.
--
-- /Note:/ Consider using 'notificationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpNotificationOptions :: Lens.Lens' AddResourcePermissions (Core.Maybe Types.NotificationOptions)
arpNotificationOptions = Lens.field @"notificationOptions"
{-# INLINEABLE arpNotificationOptions #-}
{-# DEPRECATED notificationOptions "Use generic-lens or generic-optics with 'notificationOptions' instead"  #-}

instance Core.ToQuery AddResourcePermissions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddResourcePermissions where
        toHeaders AddResourcePermissions{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddResourcePermissions where
        toJSON AddResourcePermissions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Principals" Core..= principals),
                  ("NotificationOptions" Core..=) Core.<$> notificationOptions])

instance Core.AWSRequest AddResourcePermissions where
        type Rs AddResourcePermissions = AddResourcePermissionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/api/v1/resources/" Core.<> Core.toText resourceId Core.<>
                             "/permissions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddResourcePermissionsResponse' Core.<$>
                   (x Core..:? "ShareResults") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddResourcePermissionsResponse' smart constructor.
data AddResourcePermissionsResponse = AddResourcePermissionsResponse'
  { shareResults :: Core.Maybe [Types.ShareResult]
    -- ^ The share results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddResourcePermissionsResponse' value with any optional fields omitted.
mkAddResourcePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddResourcePermissionsResponse
mkAddResourcePermissionsResponse responseStatus
  = AddResourcePermissionsResponse'{shareResults = Core.Nothing,
                                    responseStatus}

-- | The share results.
--
-- /Note:/ Consider using 'shareResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arprrsShareResults :: Lens.Lens' AddResourcePermissionsResponse (Core.Maybe [Types.ShareResult])
arprrsShareResults = Lens.field @"shareResults"
{-# INLINEABLE arprrsShareResults #-}
{-# DEPRECATED shareResults "Use generic-lens or generic-optics with 'shareResults' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arprrsResponseStatus :: Lens.Lens' AddResourcePermissionsResponse Core.Int
arprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE arprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
