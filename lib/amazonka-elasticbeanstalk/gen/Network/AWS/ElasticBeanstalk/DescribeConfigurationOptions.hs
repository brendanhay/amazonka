{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeConfigurationOptions (..),
    mkDescribeConfigurationOptions,

    -- ** Request lenses
    dcoApplicationName,
    dcoEnvironmentName,
    dcoOptions,
    dcoPlatformArn,
    dcoSolutionStackName,
    dcoTemplateName,

    -- * Destructuring the response
    DescribeConfigurationOptionsResponse (..),
    mkDescribeConfigurationOptionsResponse,

    -- ** Response lenses
    dcorrsOptions,
    dcorrsPlatformArn,
    dcorrsSolutionStackName,
    dcorrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Result message containing a list of application version descriptions.
--
-- /See:/ 'mkDescribeConfigurationOptions' smart constructor.
data DescribeConfigurationOptions = DescribeConfigurationOptions'
  { -- | The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The name of the environment whose configuration options you want to describe.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | If specified, restricts the descriptions to only the specified options.
    options :: Core.Maybe [Types.OptionSpecification],
    -- | The ARN of the custom platform.
    platformArn :: Core.Maybe Types.PlatformArn,
    -- | The name of the solution stack whose configuration options you want to describe.
    solutionStackName :: Core.Maybe Types.SolutionStackName,
    -- | The name of the configuration template whose configuration options you want to describe.
    templateName :: Core.Maybe Types.TemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationOptions' value with any optional fields omitted.
mkDescribeConfigurationOptions ::
  DescribeConfigurationOptions
mkDescribeConfigurationOptions =
  DescribeConfigurationOptions'
    { applicationName = Core.Nothing,
      environmentName = Core.Nothing,
      options = Core.Nothing,
      platformArn = Core.Nothing,
      solutionStackName = Core.Nothing,
      templateName = Core.Nothing
    }

-- | The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoApplicationName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.ApplicationName)
dcoApplicationName = Lens.field @"applicationName"
{-# DEPRECATED dcoApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the environment whose configuration options you want to describe.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoEnvironmentName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.EnvironmentName)
dcoEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED dcoEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | If specified, restricts the descriptions to only the specified options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoOptions :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe [Types.OptionSpecification])
dcoOptions = Lens.field @"options"
{-# DEPRECATED dcoOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The ARN of the custom platform.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoPlatformArn :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.PlatformArn)
dcoPlatformArn = Lens.field @"platformArn"
{-# DEPRECATED dcoPlatformArn "Use generic-lens or generic-optics with 'platformArn' instead." #-}

-- | The name of the solution stack whose configuration options you want to describe.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoSolutionStackName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.SolutionStackName)
dcoSolutionStackName = Lens.field @"solutionStackName"
{-# DEPRECATED dcoSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | The name of the configuration template whose configuration options you want to describe.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoTemplateName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Types.TemplateName)
dcoTemplateName = Lens.field @"templateName"
{-# DEPRECATED dcoTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.AWSRequest DescribeConfigurationOptions where
  type
    Rs DescribeConfigurationOptions =
      DescribeConfigurationOptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeConfigurationOptions")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" Core.<$> applicationName)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> ( Core.toQueryValue
                            "Options"
                            (Core.toQueryList "member" Core.<$> options)
                        )
                Core.<> (Core.toQueryValue "PlatformArn" Core.<$> platformArn)
                Core.<> (Core.toQueryValue "SolutionStackName" Core.<$> solutionStackName)
                Core.<> (Core.toQueryValue "TemplateName" Core.<$> templateName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationOptionsResult"
      ( \s h x ->
          DescribeConfigurationOptionsResponse'
            Core.<$> (x Core..@? "Options" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "PlatformArn")
            Core.<*> (x Core..@? "SolutionStackName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Describes the settings for a specified configuration set.
--
-- /See:/ 'mkDescribeConfigurationOptionsResponse' smart constructor.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'
  { -- | A list of 'ConfigurationOptionDescription' .
    options :: Core.Maybe [Types.ConfigurationOptionDescription],
    -- | The ARN of the platform version.
    platformArn :: Core.Maybe Types.PlatformArn,
    -- | The name of the solution stack these configuration options belong to.
    solutionStackName :: Core.Maybe Types.SolutionStackName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationOptionsResponse' value with any optional fields omitted.
mkDescribeConfigurationOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConfigurationOptionsResponse
mkDescribeConfigurationOptionsResponse responseStatus =
  DescribeConfigurationOptionsResponse'
    { options = Core.Nothing,
      platformArn = Core.Nothing,
      solutionStackName = Core.Nothing,
      responseStatus
    }

-- | A list of 'ConfigurationOptionDescription' .
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsOptions :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe [Types.ConfigurationOptionDescription])
dcorrsOptions = Lens.field @"options"
{-# DEPRECATED dcorrsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsPlatformArn :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe Types.PlatformArn)
dcorrsPlatformArn = Lens.field @"platformArn"
{-# DEPRECATED dcorrsPlatformArn "Use generic-lens or generic-optics with 'platformArn' instead." #-}

-- | The name of the solution stack these configuration options belong to.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsSolutionStackName :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe Types.SolutionStackName)
dcorrsSolutionStackName = Lens.field @"solutionStackName"
{-# DEPRECATED dcorrsSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorrsResponseStatus :: Lens.Lens' DescribeConfigurationOptionsResponse Core.Int
dcorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
