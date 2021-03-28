{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLaunchTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more versions of a launch template. You cannot delete the default version of a launch template; you must first assign a different version as the default. If the default version is the only version for the launch template, you must delete the entire launch template using 'DeleteLaunchTemplate' .
module Network.AWS.EC2.DeleteLaunchTemplateVersions
    (
    -- * Creating a request
      DeleteLaunchTemplateVersions (..)
    , mkDeleteLaunchTemplateVersions
    -- ** Request lenses
    , dltvVersions
    , dltvDryRun
    , dltvLaunchTemplateId
    , dltvLaunchTemplateName

    -- * Destructuring the response
    , DeleteLaunchTemplateVersionsResponse (..)
    , mkDeleteLaunchTemplateVersionsResponse
    -- ** Response lenses
    , dltvrrsSuccessfullyDeletedLaunchTemplateVersions
    , dltvrrsUnsuccessfullyDeletedLaunchTemplateVersions
    , dltvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLaunchTemplateVersions' smart constructor.
data DeleteLaunchTemplateVersions = DeleteLaunchTemplateVersions'
  { versions :: [Core.Text]
    -- ^ The version numbers of one or more launch template versions to delete.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , launchTemplateId :: Core.Maybe Types.LaunchTemplateId
    -- ^ The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template. You must specify either the launch template ID or launch template name in the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchTemplateVersions' value with any optional fields omitted.
mkDeleteLaunchTemplateVersions
    :: DeleteLaunchTemplateVersions
mkDeleteLaunchTemplateVersions
  = DeleteLaunchTemplateVersions'{versions = Core.mempty,
                                  dryRun = Core.Nothing, launchTemplateId = Core.Nothing,
                                  launchTemplateName = Core.Nothing}

-- | The version numbers of one or more launch template versions to delete.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvVersions :: Lens.Lens' DeleteLaunchTemplateVersions [Core.Text]
dltvVersions = Lens.field @"versions"
{-# INLINEABLE dltvVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvDryRun :: Lens.Lens' DeleteLaunchTemplateVersions (Core.Maybe Core.Bool)
dltvDryRun = Lens.field @"dryRun"
{-# INLINEABLE dltvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersions (Core.Maybe Types.LaunchTemplateId)
dltvLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE dltvLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersions (Core.Maybe Types.LaunchTemplateName)
dltvLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE dltvLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

instance Core.ToQuery DeleteLaunchTemplateVersions where
        toQuery DeleteLaunchTemplateVersions{..}
          = Core.toQueryPair "Action"
              ("DeleteLaunchTemplateVersions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "LaunchTemplateVersion" versions
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateId")
                launchTemplateId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateName")
                launchTemplateName

instance Core.ToHeaders DeleteLaunchTemplateVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteLaunchTemplateVersions where
        type Rs DeleteLaunchTemplateVersions =
             DeleteLaunchTemplateVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DeleteLaunchTemplateVersionsResponse' Core.<$>
                   (x Core..@? "successfullyDeletedLaunchTemplateVersionSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "unsuccessfullyDeletedLaunchTemplateVersionSet"
                       Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLaunchTemplateVersionsResponse' smart constructor.
data DeleteLaunchTemplateVersionsResponse = DeleteLaunchTemplateVersionsResponse'
  { successfullyDeletedLaunchTemplateVersions :: Core.Maybe [Types.DeleteLaunchTemplateVersionsResponseSuccessItem]
    -- ^ Information about the launch template versions that were successfully deleted.
  , unsuccessfullyDeletedLaunchTemplateVersions :: Core.Maybe [Types.DeleteLaunchTemplateVersionsResponseErrorItem]
    -- ^ Information about the launch template versions that could not be deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchTemplateVersionsResponse' value with any optional fields omitted.
mkDeleteLaunchTemplateVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLaunchTemplateVersionsResponse
mkDeleteLaunchTemplateVersionsResponse responseStatus
  = DeleteLaunchTemplateVersionsResponse'{successfullyDeletedLaunchTemplateVersions
                                            = Core.Nothing,
                                          unsuccessfullyDeletedLaunchTemplateVersions =
                                            Core.Nothing,
                                          responseStatus}

-- | Information about the launch template versions that were successfully deleted.
--
-- /Note:/ Consider using 'successfullyDeletedLaunchTemplateVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrrsSuccessfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Core.Maybe [Types.DeleteLaunchTemplateVersionsResponseSuccessItem])
dltvrrsSuccessfullyDeletedLaunchTemplateVersions = Lens.field @"successfullyDeletedLaunchTemplateVersions"
{-# INLINEABLE dltvrrsSuccessfullyDeletedLaunchTemplateVersions #-}
{-# DEPRECATED successfullyDeletedLaunchTemplateVersions "Use generic-lens or generic-optics with 'successfullyDeletedLaunchTemplateVersions' instead"  #-}

-- | Information about the launch template versions that could not be deleted.
--
-- /Note:/ Consider using 'unsuccessfullyDeletedLaunchTemplateVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrrsUnsuccessfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Core.Maybe [Types.DeleteLaunchTemplateVersionsResponseErrorItem])
dltvrrsUnsuccessfullyDeletedLaunchTemplateVersions = Lens.field @"unsuccessfullyDeletedLaunchTemplateVersions"
{-# INLINEABLE dltvrrsUnsuccessfullyDeletedLaunchTemplateVersions #-}
{-# DEPRECATED unsuccessfullyDeletedLaunchTemplateVersions "Use generic-lens or generic-optics with 'unsuccessfullyDeletedLaunchTemplateVersions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrrsResponseStatus :: Lens.Lens' DeleteLaunchTemplateVersionsResponse Core.Int
dltvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dltvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
