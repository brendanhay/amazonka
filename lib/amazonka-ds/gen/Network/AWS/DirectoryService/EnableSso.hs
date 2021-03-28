{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableSso
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables single sign-on for a directory. Single sign-on allows users in your directory to access certain AWS services from a computer joined to the directory without having to enter their credentials separately.
module Network.AWS.DirectoryService.EnableSso
    (
    -- * Creating a request
      EnableSso (..)
    , mkEnableSso
    -- ** Request lenses
    , esDirectoryId
    , esPassword
    , esUserName

    -- * Destructuring the response
    , EnableSsoResponse (..)
    , mkEnableSsoResponse
    -- ** Response lenses
    , esrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'EnableSso' operation.
--
-- /See:/ 'mkEnableSso' smart constructor.
data EnableSso = EnableSso'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory for which to enable single-sign on.
  , password :: Core.Maybe Types.ConnectPassword
    -- ^ The password of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
  , userName :: Core.Maybe Types.UserName
    -- ^ The username of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. This account must have privileges to add a service principal name.
--
-- If the AD Connector service account does not have privileges to add a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to enable single sign-on and are not stored by the service. The AD Connector service account is not changed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableSso' value with any optional fields omitted.
mkEnableSso
    :: Types.DirectoryId -- ^ 'directoryId'
    -> EnableSso
mkEnableSso directoryId
  = EnableSso'{directoryId, password = Core.Nothing,
               userName = Core.Nothing}

-- | The identifier of the directory for which to enable single-sign on.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDirectoryId :: Lens.Lens' EnableSso Types.DirectoryId
esDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE esDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The password of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPassword :: Lens.Lens' EnableSso (Core.Maybe Types.ConnectPassword)
esPassword = Lens.field @"password"
{-# INLINEABLE esPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The username of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. This account must have privileges to add a service principal name.
--
-- If the AD Connector service account does not have privileges to add a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to enable single sign-on and are not stored by the service. The AD Connector service account is not changed.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esUserName :: Lens.Lens' EnableSso (Core.Maybe Types.UserName)
esUserName = Lens.field @"userName"
{-# INLINEABLE esUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery EnableSso where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableSso where
        toHeaders EnableSso{..}
          = Core.pure ("X-Amz-Target", "DirectoryService_20150416.EnableSso")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableSso where
        toJSON EnableSso{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("Password" Core..=) Core.<$> password,
                  ("UserName" Core..=) Core.<$> userName])

instance Core.AWSRequest EnableSso where
        type Rs EnableSso = EnableSsoResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 EnableSsoResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'EnableSso' operation.
--
-- /See:/ 'mkEnableSsoResponse' smart constructor.
newtype EnableSsoResponse = EnableSsoResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableSsoResponse' value with any optional fields omitted.
mkEnableSsoResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableSsoResponse
mkEnableSsoResponse responseStatus
  = EnableSsoResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsResponseStatus :: Lens.Lens' EnableSsoResponse Core.Int
esrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE esrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
