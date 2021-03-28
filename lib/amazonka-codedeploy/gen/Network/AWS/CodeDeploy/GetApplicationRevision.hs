{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetApplicationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application revision.
module Network.AWS.CodeDeploy.GetApplicationRevision
    (
    -- * Creating a request
      GetApplicationRevision (..)
    , mkGetApplicationRevision
    -- ** Request lenses
    , garApplicationName
    , garRevision

    -- * Destructuring the response
    , GetApplicationRevisionResponse (..)
    , mkGetApplicationRevisionResponse
    -- ** Response lenses
    , garrrsApplicationName
    , garrrsRevision
    , garrrsRevisionInfo
    , garrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApplicationRevision@ operation.
--
-- /See:/ 'mkGetApplicationRevision' smart constructor.
data GetApplicationRevision = GetApplicationRevision'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application that corresponds to the revision.
  , revision :: Types.RevisionLocation
    -- ^ Information about the application revision to get, including type and location.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplicationRevision' value with any optional fields omitted.
mkGetApplicationRevision
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.RevisionLocation -- ^ 'revision'
    -> GetApplicationRevision
mkGetApplicationRevision applicationName revision
  = GetApplicationRevision'{applicationName, revision}

-- | The name of the application that corresponds to the revision.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garApplicationName :: Lens.Lens' GetApplicationRevision Types.ApplicationName
garApplicationName = Lens.field @"applicationName"
{-# INLINEABLE garApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Information about the application revision to get, including type and location.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garRevision :: Lens.Lens' GetApplicationRevision Types.RevisionLocation
garRevision = Lens.field @"revision"
{-# INLINEABLE garRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

instance Core.ToQuery GetApplicationRevision where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApplicationRevision where
        toHeaders GetApplicationRevision{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.GetApplicationRevision")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetApplicationRevision where
        toJSON GetApplicationRevision{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationName" Core..= applicationName),
                  Core.Just ("revision" Core..= revision)])

instance Core.AWSRequest GetApplicationRevision where
        type Rs GetApplicationRevision = GetApplicationRevisionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApplicationRevisionResponse' Core.<$>
                   (x Core..:? "applicationName") Core.<*> x Core..:? "revision"
                     Core.<*> x Core..:? "revisionInfo"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetApplicationRevision@ operation.
--
-- /See:/ 'mkGetApplicationRevisionResponse' smart constructor.
data GetApplicationRevisionResponse = GetApplicationRevisionResponse'
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The name of the application that corresponds to the revision.
  , revision :: Core.Maybe Types.RevisionLocation
    -- ^ Additional information about the revision, including type and location.
  , revisionInfo :: Core.Maybe Types.GenericRevisionInfo
    -- ^ General information about the revision.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetApplicationRevisionResponse' value with any optional fields omitted.
mkGetApplicationRevisionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetApplicationRevisionResponse
mkGetApplicationRevisionResponse responseStatus
  = GetApplicationRevisionResponse'{applicationName = Core.Nothing,
                                    revision = Core.Nothing, revisionInfo = Core.Nothing,
                                    responseStatus}

-- | The name of the application that corresponds to the revision.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsApplicationName :: Lens.Lens' GetApplicationRevisionResponse (Core.Maybe Types.ApplicationName)
garrrsApplicationName = Lens.field @"applicationName"
{-# INLINEABLE garrrsApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | Additional information about the revision, including type and location.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsRevision :: Lens.Lens' GetApplicationRevisionResponse (Core.Maybe Types.RevisionLocation)
garrrsRevision = Lens.field @"revision"
{-# INLINEABLE garrrsRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | General information about the revision.
--
-- /Note:/ Consider using 'revisionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsRevisionInfo :: Lens.Lens' GetApplicationRevisionResponse (Core.Maybe Types.GenericRevisionInfo)
garrrsRevisionInfo = Lens.field @"revisionInfo"
{-# INLINEABLE garrrsRevisionInfo #-}
{-# DEPRECATED revisionInfo "Use generic-lens or generic-optics with 'revisionInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrrsResponseStatus :: Lens.Lens' GetApplicationRevisionResponse Core.Int
garrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
