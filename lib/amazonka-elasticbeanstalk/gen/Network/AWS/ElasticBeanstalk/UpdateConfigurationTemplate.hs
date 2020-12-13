{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration template to have the specified properties or configuration option values.
--
-- Related Topics
--
--     * 'DescribeConfigurationOptions'
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
  ( -- * Creating a request
    UpdateConfigurationTemplate (..),
    mkUpdateConfigurationTemplate,

    -- ** Request lenses
    uctTemplateName,
    uctOptionsToRemove,
    uctOptionSettings,
    uctApplicationName,
    uctDescription,

    -- * Destructuring the response
    ConfigurationSettingsDescription (..),
    mkConfigurationSettingsDescription,

    -- ** Response lenses
    csdTemplateName,
    csdOptionSettings,
    csdDateUpdated,
    csdDateCreated,
    csdPlatformARN,
    csdEnvironmentName,
    csdApplicationName,
    csdDeploymentStatus,
    csdSolutionStackName,
    csdDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The result message containing the options for the specified solution stack.
--
-- /See:/ 'mkUpdateConfigurationTemplate' smart constructor.
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'
  { -- | The name of the configuration template to update.
    --
    -- If no configuration template is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
    templateName :: Lude.Text,
    -- | A list of configuration options to remove from the configuration set.
    --
    -- Constraint: You can remove only @UserDefined@ configuration options.
    optionsToRemove :: Lude.Maybe [OptionSpecification],
    -- | A list of configuration option settings to update with the new specified option value.
    optionSettings :: Lude.Maybe [ConfigurationOptionSetting],
    -- | The name of the application associated with the configuration template to update.
    --
    -- If no application is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
    applicationName :: Lude.Text,
    -- | A new description for the configuration.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the configuration template to update.
--
-- If no configuration template is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
-- * 'optionsToRemove' - A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
-- * 'optionSettings' - A list of configuration option settings to update with the new specified option value.
-- * 'applicationName' - The name of the application associated with the configuration template to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
-- * 'description' - A new description for the configuration.
mkUpdateConfigurationTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  UpdateConfigurationTemplate
mkUpdateConfigurationTemplate pTemplateName_ pApplicationName_ =
  UpdateConfigurationTemplate'
    { templateName = pTemplateName_,
      optionsToRemove = Lude.Nothing,
      optionSettings = Lude.Nothing,
      applicationName = pApplicationName_,
      description = Lude.Nothing
    }

-- | The name of the configuration template to update.
--
-- If no configuration template is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctTemplateName :: Lens.Lens' UpdateConfigurationTemplate Lude.Text
uctTemplateName = Lens.lens (templateName :: UpdateConfigurationTemplate -> Lude.Text) (\s a -> s {templateName = a} :: UpdateConfigurationTemplate)
{-# DEPRECATED uctTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctOptionsToRemove :: Lens.Lens' UpdateConfigurationTemplate (Lude.Maybe [OptionSpecification])
uctOptionsToRemove = Lens.lens (optionsToRemove :: UpdateConfigurationTemplate -> Lude.Maybe [OptionSpecification]) (\s a -> s {optionsToRemove = a} :: UpdateConfigurationTemplate)
{-# DEPRECATED uctOptionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead." #-}

-- | A list of configuration option settings to update with the new specified option value.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctOptionSettings :: Lens.Lens' UpdateConfigurationTemplate (Lude.Maybe [ConfigurationOptionSetting])
uctOptionSettings = Lens.lens (optionSettings :: UpdateConfigurationTemplate -> Lude.Maybe [ConfigurationOptionSetting]) (\s a -> s {optionSettings = a} :: UpdateConfigurationTemplate)
{-# DEPRECATED uctOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The name of the application associated with the configuration template to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctApplicationName :: Lens.Lens' UpdateConfigurationTemplate Lude.Text
uctApplicationName = Lens.lens (applicationName :: UpdateConfigurationTemplate -> Lude.Text) (\s a -> s {applicationName = a} :: UpdateConfigurationTemplate)
{-# DEPRECATED uctApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A new description for the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctDescription :: Lens.Lens' UpdateConfigurationTemplate (Lude.Maybe Lude.Text)
uctDescription = Lens.lens (description :: UpdateConfigurationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateConfigurationTemplate)
{-# DEPRECATED uctDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateConfigurationTemplate where
  type
    Rs UpdateConfigurationTemplate =
      ConfigurationSettingsDescription
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "UpdateConfigurationTemplateResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders UpdateConfigurationTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateConfigurationTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConfigurationTemplate where
  toQuery UpdateConfigurationTemplate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateConfigurationTemplate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "OptionsToRemove"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionsToRemove),
        "OptionSettings"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionSettings),
        "ApplicationName" Lude.=: applicationName,
        "Description" Lude.=: description
      ]
