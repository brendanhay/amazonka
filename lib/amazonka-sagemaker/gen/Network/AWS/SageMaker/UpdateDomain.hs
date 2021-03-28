{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default settings for new user profiles in the domain.
module Network.AWS.SageMaker.UpdateDomain
    (
    -- * Creating a request
      UpdateDomain (..)
    , mkUpdateDomain
    -- ** Request lenses
    , udDomainId
    , udDefaultUserSettings

    -- * Destructuring the response
    , UpdateDomainResponse (..)
    , mkUpdateDomainResponse
    -- ** Response lenses
    , udrrsDomainArn
    , udrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateDomain' smart constructor.
data UpdateDomain = UpdateDomain'
  { domainId :: Types.DomainId
    -- ^ The ID of the domain to be updated.
  , defaultUserSettings :: Core.Maybe Types.UserSettings
    -- ^ A collection of settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomain' value with any optional fields omitted.
mkUpdateDomain
    :: Types.DomainId -- ^ 'domainId'
    -> UpdateDomain
mkUpdateDomain domainId
  = UpdateDomain'{domainId, defaultUserSettings = Core.Nothing}

-- | The ID of the domain to be updated.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDomainId :: Lens.Lens' UpdateDomain Types.DomainId
udDomainId = Lens.field @"domainId"
{-# INLINEABLE udDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | A collection of settings.
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDefaultUserSettings :: Lens.Lens' UpdateDomain (Core.Maybe Types.UserSettings)
udDefaultUserSettings = Lens.field @"defaultUserSettings"
{-# INLINEABLE udDefaultUserSettings #-}
{-# DEPRECATED defaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead"  #-}

instance Core.ToQuery UpdateDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDomain where
        toHeaders UpdateDomain{..}
          = Core.pure ("X-Amz-Target", "SageMaker.UpdateDomain") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDomain where
        toJSON UpdateDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  ("DefaultUserSettings" Core..=) Core.<$> defaultUserSettings])

instance Core.AWSRequest UpdateDomain where
        type Rs UpdateDomain = UpdateDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDomainResponse' Core.<$>
                   (x Core..:? "DomainArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDomainResponse' smart constructor.
data UpdateDomainResponse = UpdateDomainResponse'
  { domainArn :: Core.Maybe Types.DomainArn
    -- ^ The Amazon Resource Name (ARN) of the domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainResponse' value with any optional fields omitted.
mkUpdateDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDomainResponse
mkUpdateDomainResponse responseStatus
  = UpdateDomainResponse'{domainArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the domain.
--
-- /Note:/ Consider using 'domainArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsDomainArn :: Lens.Lens' UpdateDomainResponse (Core.Maybe Types.DomainArn)
udrrsDomainArn = Lens.field @"domainArn"
{-# INLINEABLE udrrsDomainArn #-}
{-# DEPRECATED domainArn "Use generic-lens or generic-optics with 'domainArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDomainResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
