{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the configuration options that are used in a particular configuration template or environment, or that a specified solution stack defines. The description includes the values the options, their default values, and an indication of the required action on a running environment if an option value is changed.
module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    (
    -- * Creating a request
      DescribeConfigurationOptions (..)
    , mkDescribeConfigurationOptions
    -- ** Request lenses
    , dcoApplicationName
    , dcoEnvironmentName
    , dcoOptions
    , dcoPlatformArn
    , dcoSolutionStackName
    , dcoTemplateName

    -- * Destructuring the response
    , DescribeConfigurationOptionsResponse (..)
    , mkDescribeConfigurationOptionsResponse
    -- ** Response lenses
    , dcorrsOptions
    , dcorrsPlatformArn
    , dcorrsSolutionStackName
    , dcorrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Result message containing a list of application version descriptions.
--
-- /See:/ 'mkDescribeConfigurationOptions' smart constructor.
data DescribeConfigurationOptions = DescribeConfigurationOptions'
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment whose configuration options you want to describe.
  , options :: Core.Maybe [Types.OptionSpecification]
    -- ^ If specified, restricts the descriptions to only the specified options.
  , platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The ARN of the custom platform.
  , solutionStackName :: Core.Maybe Types.SolutionStackName
    -- ^ The name of the solution stack whose configuration options you want to describe.
  , templateName :: Core.Maybe Types.TemplateName
    -- ^ The name of the configuration template whose configuration options you want to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationOptions' value with any optional fields omitted.
mkDescribeConfigurationOptions
    :: DescribeConfigurationOptions
mkDescribeConfigurationOptions
  = DescribeConfigurationOptions'{applicationName = Core.Nothing,
                                  environmentName = Core.Nothing, options = Core.Nothing,
                                  platformArn = Core.Nothing, solutionStackName = Core.Nothing,
                                  templateName = Core.Nothing}

-- | The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoApplicationName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.ApplicationName)
dcoApplicationName = Lens.field @"applicationName"
{-# INLINEABLE dcoApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The name of the environment whose configuration options you want to describe.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoEnvironmentName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.EnvironmentName)
dcoEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE dcoEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | If specified, restricts the descriptions to only the specified options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoOptions :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe [Types.OptionSpecification])
dcoOptions = Lens.field @"options"
{-# INLINEABLE dcoOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The ARN of the custom platform.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoPlatformArn :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.PlatformArn)
dcoPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE dcoPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

-- | The name of the solution stack whose configuration options you want to describe.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoSolutionStackName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.SolutionStackName)
dcoSolutionStackName = Lens.field @"solutionStackName"
{-# INLINEABLE dcoSolutionStackName #-}
{-# DEPRECATED solutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead"  #-}

-- | The name of the configuration template whose configuration options you want to describe.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoTemplateName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.TemplateName)
dcoTemplateName = Lens.field @"templateName"
{-# INLINEABLE dcoTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery DescribeConfigurationOptions where
        toQuery DescribeConfigurationOptions{..}
          = Core.toQueryPair "Action"
              ("DescribeConfigurationOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplicationName")
                applicationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName
              Core.<>
              Core.toQueryPair "Options"
                (Core.maybe Core.mempty (Core.toQueryList "member") options)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PlatformArn") platformArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SolutionStackName")
                solutionStackName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateName")
                templateName

instance Core.ToHeaders DescribeConfigurationOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeConfigurationOptions where
        type Rs DescribeConfigurationOptions =
             DescribeConfigurationOptionsResponse
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
          = Response.receiveXMLWrapper "DescribeConfigurationOptionsResult"
              (\ s h x ->
                 DescribeConfigurationOptionsResponse' Core.<$>
                   (x Core..@? "Options" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "PlatformArn"
                     Core.<*> x Core..@? "SolutionStackName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Describes the settings for a specified configuration set.
--
-- /See:/ 'mkDescribeConfigurationOptionsResponse' smart constructor.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'
  { options :: Core.Maybe [Types.ConfigurationOptionDescription]
    -- ^ A list of 'ConfigurationOptionDescription' . 
  , platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The ARN of the platform version.
  , solutionStackName :: Core.Maybe Types.SolutionStackName
    -- ^ The name of the solution stack these configuration options belong to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationOptionsResponse' value with any optional fields omitted.
mkDescribeConfigurationOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConfigurationOptionsResponse
mkDescribeConfigurationOptionsResponse responseStatus
  = DescribeConfigurationOptionsResponse'{options = Core.Nothing,
                                          platformArn = Core.Nothing,
                                          solutionStackName = Core.Nothing, responseStatus}

-- | A list of 'ConfigurationOptionDescription' . 
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsOptions :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe [Types.ConfigurationOptionDescription])
dcorrsOptions = Lens.field @"options"
{-# INLINEABLE dcorrsOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsPlatformArn :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe Types.PlatformArn)
dcorrsPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE dcorrsPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

-- | The name of the solution stack these configuration options belong to.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsSolutionStackName :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe Types.SolutionStackName)
dcorrsSolutionStackName = Lens.field @"solutionStackName"
{-# INLINEABLE dcorrsSolutionStackName #-}
{-# DEPRECATED solutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsResponseStatus :: Lens.Lens' DescribeConfigurationOptionsResponse Core.Int
dcorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
