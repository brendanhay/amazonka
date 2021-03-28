{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy for the application.
module Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
    (
    -- * Creating a request
      GetApplicationPolicy (..)
    , mkGetApplicationPolicy
    -- ** Request lenses
    , gapApplicationId

    -- * Destructuring the response
    , GetApplicationPolicyResponse (..)
    , mkGetApplicationPolicyResponse
    -- ** Response lenses
    , gaprrsStatements
    , gaprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkGetApplicationPolicy' smart constructor.
newtype GetApplicationPolicy = GetApplicationPolicy'
  { applicationId :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplicationPolicy' value with any optional fields omitted.
mkGetApplicationPolicy
    :: Core.Text -- ^ 'applicationId'
    -> GetApplicationPolicy
mkGetApplicationPolicy applicationId
  = GetApplicationPolicy'{applicationId}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gapApplicationId :: Lens.Lens' GetApplicationPolicy Core.Text
gapApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gapApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetApplicationPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApplicationPolicy where
        toHeaders GetApplicationPolicy{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApplicationPolicy where
        type Rs GetApplicationPolicy = GetApplicationPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/applications/" Core.<> Core.toText applicationId Core.<>
                             "/policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApplicationPolicyResponse' Core.<$>
                   (x Core..:? "statements") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetApplicationPolicyResponse' smart constructor.
data GetApplicationPolicyResponse = GetApplicationPolicyResponse'
  { statements :: Core.Maybe [Types.ApplicationPolicyStatement]
    -- ^ An array of policy statements applied to the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplicationPolicyResponse' value with any optional fields omitted.
mkGetApplicationPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetApplicationPolicyResponse
mkGetApplicationPolicyResponse responseStatus
  = GetApplicationPolicyResponse'{statements = Core.Nothing,
                                  responseStatus}

-- | An array of policy statements applied to the application.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaprrsStatements :: Lens.Lens' GetApplicationPolicyResponse (Core.Maybe [Types.ApplicationPolicyStatement])
gaprrsStatements = Lens.field @"statements"
{-# INLINEABLE gaprrsStatements #-}
{-# DEPRECATED statements "Use generic-lens or generic-optics with 'statements' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaprrsResponseStatus :: Lens.Lens' GetApplicationPolicyResponse Core.Int
gaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
