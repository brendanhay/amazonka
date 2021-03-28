{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template version.
module Network.AWS.IoT.DescribeProvisioningTemplateVersion
    (
    -- * Creating a request
      DescribeProvisioningTemplateVersion (..)
    , mkDescribeProvisioningTemplateVersion
    -- ** Request lenses
    , dptvTemplateName
    , dptvVersionId

    -- * Destructuring the response
    , DescribeProvisioningTemplateVersionResponse (..)
    , mkDescribeProvisioningTemplateVersionResponse
    -- ** Response lenses
    , dptvrrsCreationDate
    , dptvrrsIsDefaultVersion
    , dptvrrsTemplateBody
    , dptvrrsVersionId
    , dptvrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProvisioningTemplateVersion' smart constructor.
data DescribeProvisioningTemplateVersion = DescribeProvisioningTemplateVersion'
  { templateName :: Types.TemplateName
    -- ^ The template name.
  , versionId :: Core.Int
    -- ^ The fleet provisioning template version ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisioningTemplateVersion' value with any optional fields omitted.
mkDescribeProvisioningTemplateVersion
    :: Types.TemplateName -- ^ 'templateName'
    -> Core.Int -- ^ 'versionId'
    -> DescribeProvisioningTemplateVersion
mkDescribeProvisioningTemplateVersion templateName versionId
  = DescribeProvisioningTemplateVersion'{templateName, versionId}

-- | The template name.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvTemplateName :: Lens.Lens' DescribeProvisioningTemplateVersion Types.TemplateName
dptvTemplateName = Lens.field @"templateName"
{-# INLINEABLE dptvTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The fleet provisioning template version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvVersionId :: Lens.Lens' DescribeProvisioningTemplateVersion Core.Int
dptvVersionId = Lens.field @"versionId"
{-# INLINEABLE dptvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery DescribeProvisioningTemplateVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProvisioningTemplateVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeProvisioningTemplateVersion where
        type Rs DescribeProvisioningTemplateVersion =
             DescribeProvisioningTemplateVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/provisioning-templates/" Core.<> Core.toText templateName Core.<>
                             "/versions/"
                             Core.<> Core.toText versionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProvisioningTemplateVersionResponse' Core.<$>
                   (x Core..:? "creationDate") Core.<*> x Core..:? "isDefaultVersion"
                     Core.<*> x Core..:? "templateBody"
                     Core.<*> x Core..:? "versionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeProvisioningTemplateVersionResponse' smart constructor.
data DescribeProvisioningTemplateVersionResponse = DescribeProvisioningTemplateVersionResponse'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the fleet provisioning template version was created.
  , isDefaultVersion :: Core.Maybe Core.Bool
    -- ^ True if the fleet provisioning template version is the default version.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ The JSON formatted contents of the fleet provisioning template version.
  , versionId :: Core.Maybe Core.Int
    -- ^ The fleet provisioning template version ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeProvisioningTemplateVersionResponse' value with any optional fields omitted.
mkDescribeProvisioningTemplateVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeProvisioningTemplateVersionResponse
mkDescribeProvisioningTemplateVersionResponse responseStatus
  = DescribeProvisioningTemplateVersionResponse'{creationDate =
                                                   Core.Nothing,
                                                 isDefaultVersion = Core.Nothing,
                                                 templateBody = Core.Nothing,
                                                 versionId = Core.Nothing, responseStatus}

-- | The date when the fleet provisioning template version was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsCreationDate :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Core.NominalDiffTime)
dptvrrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE dptvrrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | True if the fleet provisioning template version is the default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsIsDefaultVersion :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Core.Bool)
dptvrrsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# INLINEABLE dptvrrsIsDefaultVersion #-}
{-# DEPRECATED isDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead"  #-}

-- | The JSON formatted contents of the fleet provisioning template version.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsTemplateBody :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Types.TemplateBody)
dptvrrsTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE dptvrrsTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | The fleet provisioning template version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsVersionId :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Core.Int)
dptvrrsVersionId = Lens.field @"versionId"
{-# INLINEABLE dptvrrsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsResponseStatus :: Lens.Lens' DescribeProvisioningTemplateVersionResponse Core.Int
dptvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dptvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
