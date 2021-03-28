{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the settings for the specified configuration set, that is, either a configuration template or the configuration set associated with a running environment.
--
-- When describing the settings for the configuration set associated with a running environment, it is possible to receive two sets of setting descriptions. One is the deployed configuration set, and the other is a draft configuration of an environment that is either in the process of deployment or that failed to deploy.
-- Related Topics
--
--     * 'DeleteEnvironmentConfiguration' 
--
--
module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
    (
    -- * Creating a request
      DescribeConfigurationSettings (..)
    , mkDescribeConfigurationSettings
    -- ** Request lenses
    , dcsApplicationName
    , dcsEnvironmentName
    , dcsTemplateName

    -- * Destructuring the response
    , DescribeConfigurationSettingsResponse (..)
    , mkDescribeConfigurationSettingsResponse
    -- ** Response lenses
    , dcsrrsConfigurationSettings
    , dcsrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Result message containing all of the configuration settings for a specified solution stack or configuration template.
--
-- /See:/ 'mkDescribeConfigurationSettings' smart constructor.
data DescribeConfigurationSettings = DescribeConfigurationSettings'
  { applicationName :: Types.ApplicationName
    -- ^ The application for the environment or configuration template.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
  , templateName :: Core.Maybe Types.TemplateName
    -- ^ The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an EnvironmentName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns a @MissingRequiredParameter@ error. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationSettings' value with any optional fields omitted.
mkDescribeConfigurationSettings
    :: Types.ApplicationName -- ^ 'applicationName'
    -> DescribeConfigurationSettings
mkDescribeConfigurationSettings applicationName
  = DescribeConfigurationSettings'{applicationName,
                                   environmentName = Core.Nothing, templateName = Core.Nothing}

-- | The application for the environment or configuration template.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsApplicationName :: Lens.Lens' DescribeConfigurationSettings Types.ApplicationName
dcsApplicationName = Lens.field @"applicationName"
{-# INLINEABLE dcsApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsEnvironmentName :: Lens.Lens' DescribeConfigurationSettings (Core.Maybe Types.EnvironmentName)
dcsEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE dcsEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an EnvironmentName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns a @MissingRequiredParameter@ error. 
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsTemplateName :: Lens.Lens' DescribeConfigurationSettings (Core.Maybe Types.TemplateName)
dcsTemplateName = Lens.field @"templateName"
{-# INLINEABLE dcsTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery DescribeConfigurationSettings where
        toQuery DescribeConfigurationSettings{..}
          = Core.toQueryPair "Action"
              ("DescribeConfigurationSettings" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateName")
                templateName

instance Core.ToHeaders DescribeConfigurationSettings where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeConfigurationSettings where
        type Rs DescribeConfigurationSettings =
             DescribeConfigurationSettingsResponse
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
          = Response.receiveXMLWrapper "DescribeConfigurationSettingsResult"
              (\ s h x ->
                 DescribeConfigurationSettingsResponse' Core.<$>
                   (x Core..@? "ConfigurationSettings" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The results from a request to change the configuration settings of an environment.
--
-- /See:/ 'mkDescribeConfigurationSettingsResponse' smart constructor.
data DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse'
  { configurationSettings :: Core.Maybe [Types.ConfigurationSettingsDescription]
    -- ^ A list of 'ConfigurationSettingsDescription' . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeConfigurationSettingsResponse' value with any optional fields omitted.
mkDescribeConfigurationSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConfigurationSettingsResponse
mkDescribeConfigurationSettingsResponse responseStatus
  = DescribeConfigurationSettingsResponse'{configurationSettings =
                                             Core.Nothing,
                                           responseStatus}

-- | A list of 'ConfigurationSettingsDescription' . 
--
-- /Note:/ Consider using 'configurationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsConfigurationSettings :: Lens.Lens' DescribeConfigurationSettingsResponse (Core.Maybe [Types.ConfigurationSettingsDescription])
dcsrrsConfigurationSettings = Lens.field @"configurationSettings"
{-# INLINEABLE dcsrrsConfigurationSettings #-}
{-# DEPRECATED configurationSettings "Use generic-lens or generic-optics with 'configurationSettings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DescribeConfigurationSettingsResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
