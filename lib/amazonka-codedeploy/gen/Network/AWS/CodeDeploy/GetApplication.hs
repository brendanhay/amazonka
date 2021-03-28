{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application.
module Network.AWS.CodeDeploy.GetApplication
    (
    -- * Creating a request
      GetApplication (..)
    , mkGetApplication
    -- ** Request lenses
    , gaApplicationName

    -- * Destructuring the response
    , GetApplicationResponse (..)
    , mkGetApplicationResponse
    -- ** Response lenses
    , garrsApplication
    , garrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApplication@ operation.
--
-- /See:/ 'mkGetApplication' smart constructor.
newtype GetApplication = GetApplication'
  { applicationName :: Types.ApplicationName
    -- ^ The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplication' value with any optional fields omitted.
mkGetApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> GetApplication
mkGetApplication applicationName = GetApplication'{applicationName}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApplicationName :: Lens.Lens' GetApplication Types.ApplicationName
gaApplicationName = Lens.field @"applicationName"
{-# INLINEABLE gaApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

instance Core.ToQuery GetApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApplication where
        toHeaders GetApplication{..}
          = Core.pure ("X-Amz-Target", "CodeDeploy_20141006.GetApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetApplication where
        toJSON GetApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationName" Core..= applicationName)])

instance Core.AWSRequest GetApplication where
        type Rs GetApplication = GetApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApplicationResponse' Core.<$>
                   (x Core..:? "application") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetApplication@ operation.
--
-- /See:/ 'mkGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { application :: Core.Maybe Types.ApplicationInfo
    -- ^ Information about the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetApplicationResponse' value with any optional fields omitted.
mkGetApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetApplicationResponse
mkGetApplicationResponse responseStatus
  = GetApplicationResponse'{application = Core.Nothing,
                            responseStatus}

-- | Information about the application.
--
-- /Note:/ Consider using 'application' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsApplication :: Lens.Lens' GetApplicationResponse (Core.Maybe Types.ApplicationInfo)
garrsApplication = Lens.field @"application"
{-# INLINEABLE garrsApplication #-}
{-# DEPRECATED application "Use generic-lens or generic-optics with 'application' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetApplicationResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
