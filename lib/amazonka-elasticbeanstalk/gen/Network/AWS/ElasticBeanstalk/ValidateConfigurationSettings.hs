{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes a set of configuration settings and either a configuration template or environment, and determines whether those values are valid.
--
-- This action returns a list of messages indicating any errors or warnings associated with the selection of option values.
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
  ( -- * Creating a request
    ValidateConfigurationSettings (..),
    mkValidateConfigurationSettings,

    -- ** Request lenses
    vcsTemplateName,
    vcsOptionSettings,
    vcsEnvironmentName,
    vcsApplicationName,

    -- * Destructuring the response
    ValidateConfigurationSettingsResponse (..),
    mkValidateConfigurationSettingsResponse,

    -- ** Response lenses
    vcsrsMessages,
    vcsrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A list of validation messages for a specified configuration template.
--
-- /See:/ 'mkValidateConfigurationSettings' smart constructor.
data ValidateConfigurationSettings = ValidateConfigurationSettings'
  { -- | The name of the configuration template to validate the settings against.
    --
    -- Condition: You cannot specify both this and an environment name.
    templateName :: Lude.Maybe Lude.Text,
    -- | A list of the options and desired values to evaluate.
    optionSettings :: [ConfigurationOptionSetting],
    -- | The name of the environment to validate the settings against.
    --
    -- Condition: You cannot specify both this and a configuration template name.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The name of the application that the configuration template or environment belongs to.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateConfigurationSettings' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
-- * 'optionSettings' - A list of the options and desired values to evaluate.
-- * 'environmentName' - The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template name.
-- * 'applicationName' - The name of the application that the configuration template or environment belongs to.
mkValidateConfigurationSettings ::
  -- | 'applicationName'
  Lude.Text ->
  ValidateConfigurationSettings
mkValidateConfigurationSettings pApplicationName_ =
  ValidateConfigurationSettings'
    { templateName = Lude.Nothing,
      optionSettings = Lude.mempty,
      environmentName = Lude.Nothing,
      applicationName = pApplicationName_
    }

-- | The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsTemplateName :: Lens.Lens' ValidateConfigurationSettings (Lude.Maybe Lude.Text)
vcsTemplateName = Lens.lens (templateName :: ValidateConfigurationSettings -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: ValidateConfigurationSettings)
{-# DEPRECATED vcsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A list of the options and desired values to evaluate.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsOptionSettings :: Lens.Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
vcsOptionSettings = Lens.lens (optionSettings :: ValidateConfigurationSettings -> [ConfigurationOptionSetting]) (\s a -> s {optionSettings = a} :: ValidateConfigurationSettings)
{-# DEPRECATED vcsOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template name.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsEnvironmentName :: Lens.Lens' ValidateConfigurationSettings (Lude.Maybe Lude.Text)
vcsEnvironmentName = Lens.lens (environmentName :: ValidateConfigurationSettings -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: ValidateConfigurationSettings)
{-# DEPRECATED vcsEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the application that the configuration template or environment belongs to.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsApplicationName :: Lens.Lens' ValidateConfigurationSettings Lude.Text
vcsApplicationName = Lens.lens (applicationName :: ValidateConfigurationSettings -> Lude.Text) (\s a -> s {applicationName = a} :: ValidateConfigurationSettings)
{-# DEPRECATED vcsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest ValidateConfigurationSettings where
  type
    Rs ValidateConfigurationSettings =
      ValidateConfigurationSettingsResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "ValidateConfigurationSettingsResult"
      ( \s h x ->
          ValidateConfigurationSettingsResponse'
            Lude.<$> ( x Lude..@? "Messages" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ValidateConfigurationSettings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ValidateConfigurationSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery ValidateConfigurationSettings where
  toQuery ValidateConfigurationSettings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ValidateConfigurationSettings" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "OptionSettings" Lude.=: Lude.toQueryList "member" optionSettings,
        "EnvironmentName" Lude.=: environmentName,
        "ApplicationName" Lude.=: applicationName
      ]

-- | Provides a list of validation messages.
--
-- /See:/ 'mkValidateConfigurationSettingsResponse' smart constructor.
data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse'
  { -- | A list of 'ValidationMessage' .
    messages :: Lude.Maybe [ValidationMessage],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateConfigurationSettingsResponse' with the minimum fields required to make a request.
--
-- * 'messages' - A list of 'ValidationMessage' .
-- * 'responseStatus' - The response status code.
mkValidateConfigurationSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ValidateConfigurationSettingsResponse
mkValidateConfigurationSettingsResponse pResponseStatus_ =
  ValidateConfigurationSettingsResponse'
    { messages = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'ValidationMessage' .
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsrsMessages :: Lens.Lens' ValidateConfigurationSettingsResponse (Lude.Maybe [ValidationMessage])
vcsrsMessages = Lens.lens (messages :: ValidateConfigurationSettingsResponse -> Lude.Maybe [ValidationMessage]) (\s a -> s {messages = a} :: ValidateConfigurationSettingsResponse)
{-# DEPRECATED vcsrsMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsrsResponseStatus :: Lens.Lens' ValidateConfigurationSettingsResponse Lude.Int
vcsrsResponseStatus = Lens.lens (responseStatus :: ValidateConfigurationSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ValidateConfigurationSettingsResponse)
{-# DEPRECATED vcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
