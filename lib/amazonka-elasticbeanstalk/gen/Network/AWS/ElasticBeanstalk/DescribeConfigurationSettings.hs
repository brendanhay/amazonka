{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
  ( -- * Creating a request
    DescribeConfigurationSettings (..),
    mkDescribeConfigurationSettings,

    -- ** Request lenses
    dcsTemplateName,
    dcsEnvironmentName,
    dcsApplicationName,

    -- * Destructuring the response
    DescribeConfigurationSettingsResponse (..),
    mkDescribeConfigurationSettingsResponse,

    -- ** Response lenses
    dcsrsConfigurationSettings,
    dcsrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Result message containing all of the configuration settings for a specified solution stack or configuration template.
--
-- /See:/ 'mkDescribeConfigurationSettings' smart constructor.
data DescribeConfigurationSettings = DescribeConfigurationSettings'
  { templateName ::
      Lude.Maybe Lude.Text,
    environmentName ::
      Lude.Maybe Lude.Text,
    applicationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationSettings' with the minimum fields required to make a request.
--
-- * 'applicationName' - The application for the environment or configuration template.
-- * 'environmentName' - The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'templateName' - The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an EnvironmentName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns a @MissingRequiredParameter@ error.
mkDescribeConfigurationSettings ::
  -- | 'applicationName'
  Lude.Text ->
  DescribeConfigurationSettings
mkDescribeConfigurationSettings pApplicationName_ =
  DescribeConfigurationSettings'
    { templateName = Lude.Nothing,
      environmentName = Lude.Nothing,
      applicationName = pApplicationName_
    }

-- | The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an EnvironmentName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns a @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsTemplateName :: Lens.Lens' DescribeConfigurationSettings (Lude.Maybe Lude.Text)
dcsTemplateName = Lens.lens (templateName :: DescribeConfigurationSettings -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: DescribeConfigurationSettings)
{-# DEPRECATED dcsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsEnvironmentName :: Lens.Lens' DescribeConfigurationSettings (Lude.Maybe Lude.Text)
dcsEnvironmentName = Lens.lens (environmentName :: DescribeConfigurationSettings -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeConfigurationSettings)
{-# DEPRECATED dcsEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The application for the environment or configuration template.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsApplicationName :: Lens.Lens' DescribeConfigurationSettings Lude.Text
dcsApplicationName = Lens.lens (applicationName :: DescribeConfigurationSettings -> Lude.Text) (\s a -> s {applicationName = a} :: DescribeConfigurationSettings)
{-# DEPRECATED dcsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DescribeConfigurationSettings where
  type
    Rs DescribeConfigurationSettings =
      DescribeConfigurationSettingsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeConfigurationSettingsResult"
      ( \s h x ->
          DescribeConfigurationSettingsResponse'
            Lude.<$> ( x Lude..@? "ConfigurationSettings" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigurationSettings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeConfigurationSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigurationSettings where
  toQuery DescribeConfigurationSettings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeConfigurationSettings" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "EnvironmentName" Lude.=: environmentName,
        "ApplicationName" Lude.=: applicationName
      ]

-- | The results from a request to change the configuration settings of an environment.
--
-- /See:/ 'mkDescribeConfigurationSettingsResponse' smart constructor.
data DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse'
  { configurationSettings ::
      Lude.Maybe
        [ConfigurationSettingsDescription],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigurationSettingsResponse' with the minimum fields required to make a request.
--
-- * 'configurationSettings' - A list of 'ConfigurationSettingsDescription' .
-- * 'responseStatus' - The response status code.
mkDescribeConfigurationSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigurationSettingsResponse
mkDescribeConfigurationSettingsResponse pResponseStatus_ =
  DescribeConfigurationSettingsResponse'
    { configurationSettings =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'ConfigurationSettingsDescription' .
--
-- /Note:/ Consider using 'configurationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsConfigurationSettings :: Lens.Lens' DescribeConfigurationSettingsResponse (Lude.Maybe [ConfigurationSettingsDescription])
dcsrsConfigurationSettings = Lens.lens (configurationSettings :: DescribeConfigurationSettingsResponse -> Lude.Maybe [ConfigurationSettingsDescription]) (\s a -> s {configurationSettings = a} :: DescribeConfigurationSettingsResponse)
{-# DEPRECATED dcsrsConfigurationSettings "Use generic-lens or generic-optics with 'configurationSettings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsResponseStatus :: Lens.Lens' DescribeConfigurationSettingsResponse Lude.Int
dcsrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigurationSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigurationSettingsResponse)
{-# DEPRECATED dcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
