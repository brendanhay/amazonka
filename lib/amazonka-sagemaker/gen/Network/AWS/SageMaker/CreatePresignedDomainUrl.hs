{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreatePresignedDomainUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL for a specified UserProfile in a Domain. When accessed in a web browser, the user will be automatically signed in to Amazon SageMaker Studio, and granted access to all of the Apps and files associated with the Domain's Amazon Elastic File System (EFS) volume. This operation can only be called when the authentication mode equals IAM. 
module Network.AWS.SageMaker.CreatePresignedDomainUrl
    (
    -- * Creating a request
      CreatePresignedDomainUrl (..)
    , mkCreatePresignedDomainUrl
    -- ** Request lenses
    , cpduDomainId
    , cpduUserProfileName
    , cpduSessionExpirationDurationInSeconds

    -- * Destructuring the response
    , CreatePresignedDomainUrlResponse (..)
    , mkCreatePresignedDomainUrlResponse
    -- ** Response lenses
    , cpdurrsAuthorizedUrl
    , cpdurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreatePresignedDomainUrl' smart constructor.
data CreatePresignedDomainUrl = CreatePresignedDomainUrl'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  , userProfileName :: Types.UserProfileName
    -- ^ The name of the UserProfile to sign-in as.
  , sessionExpirationDurationInSeconds :: Core.Maybe Core.Natural
    -- ^ The session expiration duration in seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePresignedDomainUrl' value with any optional fields omitted.
mkCreatePresignedDomainUrl
    :: Types.DomainId -- ^ 'domainId'
    -> Types.UserProfileName -- ^ 'userProfileName'
    -> CreatePresignedDomainUrl
mkCreatePresignedDomainUrl domainId userProfileName
  = CreatePresignedDomainUrl'{domainId, userProfileName,
                              sessionExpirationDurationInSeconds = Core.Nothing}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpduDomainId :: Lens.Lens' CreatePresignedDomainUrl Types.DomainId
cpduDomainId = Lens.field @"domainId"
{-# INLINEABLE cpduDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The name of the UserProfile to sign-in as.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpduUserProfileName :: Lens.Lens' CreatePresignedDomainUrl Types.UserProfileName
cpduUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE cpduUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | The session expiration duration in seconds.
--
-- /Note:/ Consider using 'sessionExpirationDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpduSessionExpirationDurationInSeconds :: Lens.Lens' CreatePresignedDomainUrl (Core.Maybe Core.Natural)
cpduSessionExpirationDurationInSeconds = Lens.field @"sessionExpirationDurationInSeconds"
{-# INLINEABLE cpduSessionExpirationDurationInSeconds #-}
{-# DEPRECATED sessionExpirationDurationInSeconds "Use generic-lens or generic-optics with 'sessionExpirationDurationInSeconds' instead"  #-}

instance Core.ToQuery CreatePresignedDomainUrl where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePresignedDomainUrl where
        toHeaders CreatePresignedDomainUrl{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreatePresignedDomainUrl")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePresignedDomainUrl where
        toJSON CreatePresignedDomainUrl{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  Core.Just ("UserProfileName" Core..= userProfileName),
                  ("SessionExpirationDurationInSeconds" Core..=) Core.<$>
                    sessionExpirationDurationInSeconds])

instance Core.AWSRequest CreatePresignedDomainUrl where
        type Rs CreatePresignedDomainUrl = CreatePresignedDomainUrlResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePresignedDomainUrlResponse' Core.<$>
                   (x Core..:? "AuthorizedUrl") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePresignedDomainUrlResponse' smart constructor.
data CreatePresignedDomainUrlResponse = CreatePresignedDomainUrlResponse'
  { authorizedUrl :: Core.Maybe Types.PresignedDomainUrl
    -- ^ The presigned URL.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePresignedDomainUrlResponse' value with any optional fields omitted.
mkCreatePresignedDomainUrlResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePresignedDomainUrlResponse
mkCreatePresignedDomainUrlResponse responseStatus
  = CreatePresignedDomainUrlResponse'{authorizedUrl = Core.Nothing,
                                      responseStatus}

-- | The presigned URL.
--
-- /Note:/ Consider using 'authorizedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdurrsAuthorizedUrl :: Lens.Lens' CreatePresignedDomainUrlResponse (Core.Maybe Types.PresignedDomainUrl)
cpdurrsAuthorizedUrl = Lens.field @"authorizedUrl"
{-# INLINEABLE cpdurrsAuthorizedUrl #-}
{-# DEPRECATED authorizedUrl "Use generic-lens or generic-optics with 'authorizedUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdurrsResponseStatus :: Lens.Lens' CreatePresignedDomainUrlResponse Core.Int
cpdurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpdurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
