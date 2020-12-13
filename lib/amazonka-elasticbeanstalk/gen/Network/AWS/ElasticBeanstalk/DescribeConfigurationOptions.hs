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
    dcoTemplateName,
    dcoPlatformARN,
    dcoEnvironmentName,
    dcoApplicationName,
    dcoSolutionStackName,
    dcoOptions,

    -- * Destructuring the response
    DescribeConfigurationOptionsResponse (..),
    mkDescribeConfigurationOptionsResponse,

    -- ** Response lenses
    dcorsPlatformARN,
    dcorsSolutionStackName,
    dcorsOptions,
    dcorsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Result message containing a list of application version descriptions.
--
-- /See:/ 'mkDescribeConfigurationOptions' smart constructor.
data DescribeConfigurationOptions = DescribeConfigurationOptions'
  { -- | The name of the configuration template whose configuration options you want to describe.
    templateName :: Lude.Maybe Lude.Text,
    -- | The ARN of the custom platform.
    platformARN :: Lude.Maybe Lude.Text,
    -- | The name of the environment whose configuration options you want to describe.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
    applicationName :: Lude.Maybe Lude.Text,
    -- | The name of the solution stack whose configuration options you want to describe.
    solutionStackName :: Lude.Maybe Lude.Text,
    -- | If specified, restricts the descriptions to only the specified options.
    options :: Lude.Maybe [OptionSpecification]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationOptions' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the configuration template whose configuration options you want to describe.
-- * 'platformARN' - The ARN of the custom platform.
-- * 'environmentName' - The name of the environment whose configuration options you want to describe.
-- * 'applicationName' - The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
-- * 'solutionStackName' - The name of the solution stack whose configuration options you want to describe.
-- * 'options' - If specified, restricts the descriptions to only the specified options.
mkDescribeConfigurationOptions ::
  DescribeConfigurationOptions
mkDescribeConfigurationOptions =
  DescribeConfigurationOptions'
    { templateName = Lude.Nothing,
      platformARN = Lude.Nothing,
      environmentName = Lude.Nothing,
      applicationName = Lude.Nothing,
      solutionStackName = Lude.Nothing,
      options = Lude.Nothing
    }

-- | The name of the configuration template whose configuration options you want to describe.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoTemplateName :: Lens.Lens' DescribeConfigurationOptions (Lude.Maybe Lude.Text)
dcoTemplateName = Lens.lens (templateName :: DescribeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: DescribeConfigurationOptions)
{-# DEPRECATED dcoTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The ARN of the custom platform.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoPlatformARN :: Lens.Lens' DescribeConfigurationOptions (Lude.Maybe Lude.Text)
dcoPlatformARN = Lens.lens (platformARN :: DescribeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: DescribeConfigurationOptions)
{-# DEPRECATED dcoPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | The name of the environment whose configuration options you want to describe.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoEnvironmentName :: Lens.Lens' DescribeConfigurationOptions (Lude.Maybe Lude.Text)
dcoEnvironmentName = Lens.lens (environmentName :: DescribeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeConfigurationOptions)
{-# DEPRECATED dcoEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoApplicationName :: Lens.Lens' DescribeConfigurationOptions (Lude.Maybe Lude.Text)
dcoApplicationName = Lens.lens (applicationName :: DescribeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: DescribeConfigurationOptions)
{-# DEPRECATED dcoApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the solution stack whose configuration options you want to describe.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoSolutionStackName :: Lens.Lens' DescribeConfigurationOptions (Lude.Maybe Lude.Text)
dcoSolutionStackName = Lens.lens (solutionStackName :: DescribeConfigurationOptions -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: DescribeConfigurationOptions)
{-# DEPRECATED dcoSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | If specified, restricts the descriptions to only the specified options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcoOptions :: Lens.Lens' DescribeConfigurationOptions (Lude.Maybe [OptionSpecification])
dcoOptions = Lens.lens (options :: DescribeConfigurationOptions -> Lude.Maybe [OptionSpecification]) (\s a -> s {options = a} :: DescribeConfigurationOptions)
{-# DEPRECATED dcoOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.AWSRequest DescribeConfigurationOptions where
  type
    Rs DescribeConfigurationOptions =
      DescribeConfigurationOptionsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeConfigurationOptionsResult"
      ( \s h x ->
          DescribeConfigurationOptionsResponse'
            Lude.<$> (x Lude..@? "PlatformArn")
            Lude.<*> (x Lude..@? "SolutionStackName")
            Lude.<*> ( x Lude..@? "Options" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurationOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeConfigurationOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurationOptions where
  toQuery DescribeConfigurationOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeConfigurationOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "PlatformArn" Lude.=: platformARN,
        "EnvironmentName" Lude.=: environmentName,
        "ApplicationName" Lude.=: applicationName,
        "SolutionStackName" Lude.=: solutionStackName,
        "Options"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> options)
      ]

-- | Describes the settings for a specified configuration set.
--
-- /See:/ 'mkDescribeConfigurationOptionsResponse' smart constructor.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'
  { -- | The ARN of the platform version.
    platformARN :: Lude.Maybe Lude.Text,
    -- | The name of the solution stack these configuration options belong to.
    solutionStackName :: Lude.Maybe Lude.Text,
    -- | A list of 'ConfigurationOptionDescription' .
    options :: Lude.Maybe [ConfigurationOptionDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationOptionsResponse' with the minimum fields required to make a request.
--
-- * 'platformARN' - The ARN of the platform version.
-- * 'solutionStackName' - The name of the solution stack these configuration options belong to.
-- * 'options' - A list of 'ConfigurationOptionDescription' .
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationOptionsResponse
mkDescribeConfigurationOptionsResponse pResponseStatus_ =
  DescribeConfigurationOptionsResponse'
    { platformARN = Lude.Nothing,
      solutionStackName = Lude.Nothing,
      options = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorsPlatformARN :: Lens.Lens' DescribeConfigurationOptionsResponse (Lude.Maybe Lude.Text)
dcorsPlatformARN = Lens.lens (platformARN :: DescribeConfigurationOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: DescribeConfigurationOptionsResponse)
{-# DEPRECATED dcorsPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | The name of the solution stack these configuration options belong to.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorsSolutionStackName :: Lens.Lens' DescribeConfigurationOptionsResponse (Lude.Maybe Lude.Text)
dcorsSolutionStackName = Lens.lens (solutionStackName :: DescribeConfigurationOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: DescribeConfigurationOptionsResponse)
{-# DEPRECATED dcorsSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | A list of 'ConfigurationOptionDescription' .
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorsOptions :: Lens.Lens' DescribeConfigurationOptionsResponse (Lude.Maybe [ConfigurationOptionDescription])
dcorsOptions = Lens.lens (options :: DescribeConfigurationOptionsResponse -> Lude.Maybe [ConfigurationOptionDescription]) (\s a -> s {options = a} :: DescribeConfigurationOptionsResponse)
{-# DEPRECATED dcorsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcorsResponseStatus :: Lens.Lens' DescribeConfigurationOptionsResponse Lude.Int
dcorsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationOptionsResponse)
{-# DEPRECATED dcorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
