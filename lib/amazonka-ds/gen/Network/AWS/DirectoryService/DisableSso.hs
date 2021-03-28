{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableSso
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables single-sign on for a directory.
module Network.AWS.DirectoryService.DisableSso
    (
    -- * Creating a request
      DisableSso (..)
    , mkDisableSso
    -- ** Request lenses
    , dsfDirectoryId
    , dsfPassword
    , dsfUserName

    -- * Destructuring the response
    , DisableSsoResponse (..)
    , mkDisableSsoResponse
    -- ** Response lenses
    , dsrgrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DisableSso' operation.
--
-- /See:/ 'mkDisableSso' smart constructor.
data DisableSso = DisableSso'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory for which to disable single-sign on.
  , password :: Core.Maybe Types.ConnectPassword
    -- ^ The password of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
  , userName :: Core.Maybe Types.UserName
    -- ^ The username of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. This account must have privileges to remove a service principal name.
--
-- If the AD Connector service account does not have privileges to remove a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to disable single sign-on and are not stored by the service. The AD Connector service account is not changed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableSso' value with any optional fields omitted.
mkDisableSso
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DisableSso
mkDisableSso directoryId
  = DisableSso'{directoryId, password = Core.Nothing,
                userName = Core.Nothing}

-- | The identifier of the directory for which to disable single-sign on.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfDirectoryId :: Lens.Lens' DisableSso Types.DirectoryId
dsfDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dsfDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The password of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfPassword :: Lens.Lens' DisableSso (Core.Maybe Types.ConnectPassword)
dsfPassword = Lens.field @"password"
{-# INLINEABLE dsfPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The username of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. This account must have privileges to remove a service principal name.
--
-- If the AD Connector service account does not have privileges to remove a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to disable single sign-on and are not stored by the service. The AD Connector service account is not changed.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfUserName :: Lens.Lens' DisableSso (Core.Maybe Types.UserName)
dsfUserName = Lens.field @"userName"
{-# INLINEABLE dsfUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery DisableSso where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableSso where
        toHeaders DisableSso{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DisableSso")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableSso where
        toJSON DisableSso{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("Password" Core..=) Core.<$> password,
                  ("UserName" Core..=) Core.<$> userName])

instance Core.AWSRequest DisableSso where
        type Rs DisableSso = DisableSsoResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisableSsoResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'DisableSso' operation.
--
-- /See:/ 'mkDisableSsoResponse' smart constructor.
newtype DisableSsoResponse = DisableSsoResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableSsoResponse' value with any optional fields omitted.
mkDisableSsoResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableSsoResponse
mkDisableSsoResponse responseStatus
  = DisableSsoResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrgrsResponseStatus :: Lens.Lens' DisableSsoResponse Core.Int
dsrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
