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
    uctApplicationName,
    uctTemplateName,
    uctDescription,
    uctOptionSettings,
    uctOptionsToRemove,

    -- * Destructuring the response
    Types.ConfigurationSettingsDescription (..),
    Types.mkConfigurationSettingsDescription,

    -- ** Response lenses
    Types.csdApplicationName,
    Types.csdDateCreated,
    Types.csdDateUpdated,
    Types.csdDeploymentStatus,
    Types.csdDescription,
    Types.csdEnvironmentName,
    Types.csdOptionSettings,
    Types.csdPlatformArn,
    Types.csdSolutionStackName,
    Types.csdTemplateName,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result message containing the options for the specified solution stack.
--
-- /See:/ 'mkUpdateConfigurationTemplate' smart constructor.
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'
  { -- | The name of the application associated with the configuration template to update.
    --
    -- If no application is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
    applicationName :: Types.ApplicationName,
    -- | The name of the configuration template to update.
    --
    -- If no configuration template is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
    templateName :: Types.TemplateName,
    -- | A new description for the configuration.
    description :: Core.Maybe Types.Description,
    -- | A list of configuration option settings to update with the new specified option value.
    optionSettings :: Core.Maybe [Types.ConfigurationOptionSetting],
    -- | A list of configuration options to remove from the configuration set.
    --
    -- Constraint: You can remove only @UserDefined@ configuration options.
    optionsToRemove :: Core.Maybe [Types.OptionSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfigurationTemplate' value with any optional fields omitted.
mkUpdateConfigurationTemplate ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'templateName'
  Types.TemplateName ->
  UpdateConfigurationTemplate
mkUpdateConfigurationTemplate applicationName templateName =
  UpdateConfigurationTemplate'
    { applicationName,
      templateName,
      description = Core.Nothing,
      optionSettings = Core.Nothing,
      optionsToRemove = Core.Nothing
    }

-- | The name of the application associated with the configuration template to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctApplicationName :: Lens.Lens' UpdateConfigurationTemplate Types.ApplicationName
uctApplicationName = Lens.field @"applicationName"
{-# DEPRECATED uctApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the configuration template to update.
--
-- If no configuration template is found with this name, @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctTemplateName :: Lens.Lens' UpdateConfigurationTemplate Types.TemplateName
uctTemplateName = Lens.field @"templateName"
{-# DEPRECATED uctTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A new description for the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctDescription :: Lens.Lens' UpdateConfigurationTemplate (Core.Maybe Types.Description)
uctDescription = Lens.field @"description"
{-# DEPRECATED uctDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of configuration option settings to update with the new specified option value.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctOptionSettings :: Lens.Lens' UpdateConfigurationTemplate (Core.Maybe [Types.ConfigurationOptionSetting])
uctOptionSettings = Lens.field @"optionSettings"
{-# DEPRECATED uctOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctOptionsToRemove :: Lens.Lens' UpdateConfigurationTemplate (Core.Maybe [Types.OptionSpecification])
uctOptionsToRemove = Lens.field @"optionsToRemove"
{-# DEPRECATED uctOptionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead." #-}

instance Core.AWSRequest UpdateConfigurationTemplate where
  type
    Rs UpdateConfigurationTemplate =
      Types.ConfigurationSettingsDescription
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
            ( Core.pure ("Action", "UpdateConfigurationTemplate")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" applicationName)
                Core.<> (Core.toQueryValue "TemplateName" templateName)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> ( Core.toQueryValue
                            "OptionSettings"
                            (Core.toQueryList "member" Core.<$> optionSettings)
                        )
                Core.<> ( Core.toQueryValue
                            "OptionsToRemove"
                            (Core.toQueryList "member" Core.<$> optionsToRemove)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateConfigurationTemplateResult"
      (\s h x -> Core.parseXML x)
