{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates permissions for the specified private image. 
module Network.AWS.AppStream.UpdateImagePermissions
    (
    -- * Creating a request
      UpdateImagePermissions (..)
    , mkUpdateImagePermissions
    -- ** Request lenses
    , uipName
    , uipSharedAccountId
    , uipImagePermissions

    -- * Destructuring the response
    , UpdateImagePermissionsResponse (..)
    , mkUpdateImagePermissionsResponse
    -- ** Response lenses
    , uiprrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateImagePermissions' smart constructor.
data UpdateImagePermissions = UpdateImagePermissions'
  { name :: Types.Name
    -- ^ The name of the private image.
  , sharedAccountId :: Types.SharedAccountId
    -- ^ The 12-digit identifier of the AWS account for which you want add or update image permissions.
  , imagePermissions :: Types.ImagePermissions
    -- ^ The permissions for the image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateImagePermissions' value with any optional fields omitted.
mkUpdateImagePermissions
    :: Types.Name -- ^ 'name'
    -> Types.SharedAccountId -- ^ 'sharedAccountId'
    -> Types.ImagePermissions -- ^ 'imagePermissions'
    -> UpdateImagePermissions
mkUpdateImagePermissions name sharedAccountId imagePermissions
  = UpdateImagePermissions'{name, sharedAccountId, imagePermissions}

-- | The name of the private image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipName :: Lens.Lens' UpdateImagePermissions Types.Name
uipName = Lens.field @"name"
{-# INLINEABLE uipName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The 12-digit identifier of the AWS account for which you want add or update image permissions.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipSharedAccountId :: Lens.Lens' UpdateImagePermissions Types.SharedAccountId
uipSharedAccountId = Lens.field @"sharedAccountId"
{-# INLINEABLE uipSharedAccountId #-}
{-# DEPRECATED sharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead"  #-}

-- | The permissions for the image.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipImagePermissions :: Lens.Lens' UpdateImagePermissions Types.ImagePermissions
uipImagePermissions = Lens.field @"imagePermissions"
{-# INLINEABLE uipImagePermissions #-}
{-# DEPRECATED imagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead"  #-}

instance Core.ToQuery UpdateImagePermissions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateImagePermissions where
        toHeaders UpdateImagePermissions{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.UpdateImagePermissions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateImagePermissions where
        toJSON UpdateImagePermissions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("SharedAccountId" Core..= sharedAccountId),
                  Core.Just ("ImagePermissions" Core..= imagePermissions)])

instance Core.AWSRequest UpdateImagePermissions where
        type Rs UpdateImagePermissions = UpdateImagePermissionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateImagePermissionsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateImagePermissionsResponse' smart constructor.
newtype UpdateImagePermissionsResponse = UpdateImagePermissionsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateImagePermissionsResponse' value with any optional fields omitted.
mkUpdateImagePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateImagePermissionsResponse
mkUpdateImagePermissionsResponse responseStatus
  = UpdateImagePermissionsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprrsResponseStatus :: Lens.Lens' UpdateImagePermissionsResponse Core.Int
uiprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uiprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
