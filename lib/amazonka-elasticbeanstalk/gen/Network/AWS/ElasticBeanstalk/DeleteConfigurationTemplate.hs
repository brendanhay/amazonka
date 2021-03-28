{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration template.
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
    (
    -- * Creating a request
      DeleteConfigurationTemplate (..)
    , mkDeleteConfigurationTemplate
    -- ** Request lenses
    , dctApplicationName
    , dctTemplateName

    -- * Destructuring the response
    , DeleteConfigurationTemplateResponse (..)
    , mkDeleteConfigurationTemplateResponse
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete a configuration template.
--
-- /See:/ 'mkDeleteConfigurationTemplate' smart constructor.
data DeleteConfigurationTemplate = DeleteConfigurationTemplate'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application to delete the configuration template from.
  , templateName :: Types.TemplateName
    -- ^ The name of the configuration template to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationTemplate' value with any optional fields omitted.
mkDeleteConfigurationTemplate
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.TemplateName -- ^ 'templateName'
    -> DeleteConfigurationTemplate
mkDeleteConfigurationTemplate applicationName templateName
  = DeleteConfigurationTemplate'{applicationName, templateName}

-- | The name of the application to delete the configuration template from.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctApplicationName :: Lens.Lens' DeleteConfigurationTemplate Types.ApplicationName
dctApplicationName = Lens.field @"applicationName"
{-# INLINEABLE dctApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The name of the configuration template to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctTemplateName :: Lens.Lens' DeleteConfigurationTemplate Types.TemplateName
dctTemplateName = Lens.field @"templateName"
{-# INLINEABLE dctTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery DeleteConfigurationTemplate where
        toQuery DeleteConfigurationTemplate{..}
          = Core.toQueryPair "Action"
              ("DeleteConfigurationTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<> Core.toQueryPair "TemplateName" templateName

instance Core.ToHeaders DeleteConfigurationTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteConfigurationTemplate where
        type Rs DeleteConfigurationTemplate =
             DeleteConfigurationTemplateResponse
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
          = Response.receiveNull DeleteConfigurationTemplateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteConfigurationTemplateResponse' smart constructor.
data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConfigurationTemplateResponse' value with any optional fields omitted.
mkDeleteConfigurationTemplateResponse
    :: DeleteConfigurationTemplateResponse
mkDeleteConfigurationTemplateResponse
  = DeleteConfigurationTemplateResponse'
