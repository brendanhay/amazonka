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
    vcsApplicationName,
    vcsOptionSettings,
    vcsEnvironmentName,
    vcsTemplateName,

    -- * Destructuring the response
    ValidateConfigurationSettingsResponse (..),
    mkValidateConfigurationSettingsResponse,

    -- ** Response lenses
    vcsrrsMessages,
    vcsrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A list of validation messages for a specified configuration template.
--
-- /See:/ 'mkValidateConfigurationSettings' smart constructor.
data ValidateConfigurationSettings = ValidateConfigurationSettings'
  { -- | The name of the application that the configuration template or environment belongs to.
    applicationName :: Types.ApplicationName,
    -- | A list of the options and desired values to evaluate.
    optionSettings :: [Types.ConfigurationOptionSetting],
    -- | The name of the environment to validate the settings against.
    --
    -- Condition: You cannot specify both this and a configuration template name.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | The name of the configuration template to validate the settings against.
    --
    -- Condition: You cannot specify both this and an environment name.
    templateName :: Core.Maybe Types.ConfigurationTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateConfigurationSettings' value with any optional fields omitted.
mkValidateConfigurationSettings ::
  -- | 'applicationName'
  Types.ApplicationName ->
  ValidateConfigurationSettings
mkValidateConfigurationSettings applicationName =
  ValidateConfigurationSettings'
    { applicationName,
      optionSettings = Core.mempty,
      environmentName = Core.Nothing,
      templateName = Core.Nothing
    }

-- | The name of the application that the configuration template or environment belongs to.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsApplicationName :: Lens.Lens' ValidateConfigurationSettings Types.ApplicationName
vcsApplicationName = Lens.field @"applicationName"
{-# DEPRECATED vcsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | A list of the options and desired values to evaluate.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsOptionSettings :: Lens.Lens' ValidateConfigurationSettings [Types.ConfigurationOptionSetting]
vcsOptionSettings = Lens.field @"optionSettings"
{-# DEPRECATED vcsOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template name.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsEnvironmentName :: Lens.Lens' ValidateConfigurationSettings (Core.Maybe Types.EnvironmentName)
vcsEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED vcsEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsTemplateName :: Lens.Lens' ValidateConfigurationSettings (Core.Maybe Types.ConfigurationTemplateName)
vcsTemplateName = Lens.field @"templateName"
{-# DEPRECATED vcsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.AWSRequest ValidateConfigurationSettings where
  type
    Rs ValidateConfigurationSettings =
      ValidateConfigurationSettingsResponse
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
            ( Core.pure ("Action", "ValidateConfigurationSettings")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" applicationName)
                Core.<> ( Core.toQueryValue
                            "OptionSettings"
                            (Core.toQueryList "member" optionSettings)
                        )
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> (Core.toQueryValue "TemplateName" Core.<$> templateName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ValidateConfigurationSettingsResult"
      ( \s h x ->
          ValidateConfigurationSettingsResponse'
            Core.<$> (x Core..@? "Messages" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Provides a list of validation messages.
--
-- /See:/ 'mkValidateConfigurationSettingsResponse' smart constructor.
data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse'
  { -- | A list of 'ValidationMessage' .
    messages :: Core.Maybe [Types.ValidationMessage],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateConfigurationSettingsResponse' value with any optional fields omitted.
mkValidateConfigurationSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ValidateConfigurationSettingsResponse
mkValidateConfigurationSettingsResponse responseStatus =
  ValidateConfigurationSettingsResponse'
    { messages = Core.Nothing,
      responseStatus
    }

-- | A list of 'ValidationMessage' .
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsrrsMessages :: Lens.Lens' ValidateConfigurationSettingsResponse (Core.Maybe [Types.ValidationMessage])
vcsrrsMessages = Lens.field @"messages"
{-# DEPRECATED vcsrrsMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcsrrsResponseStatus :: Lens.Lens' ValidateConfigurationSettingsResponse Core.Int
vcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED vcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
