{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreatePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new version of your custom platform.
module Network.AWS.ElasticBeanstalk.CreatePlatformVersion
  ( -- * Creating a request
    CreatePlatformVersion (..),
    mkCreatePlatformVersion,

    -- ** Request lenses
    cpvPlatformName,
    cpvPlatformVersion,
    cpvPlatformDefinitionBundle,
    cpvEnvironmentName,
    cpvOptionSettings,
    cpvTags,

    -- * Destructuring the response
    CreatePlatformVersionResponse (..),
    mkCreatePlatformVersionResponse,

    -- ** Response lenses
    cpvrrsBuilder,
    cpvrrsPlatformSummary,
    cpvrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create a new platform version.
--
-- /See:/ 'mkCreatePlatformVersion' smart constructor.
data CreatePlatformVersion = CreatePlatformVersion'
  { -- | The name of your custom platform.
    platformName :: Types.PlatformName,
    -- | The number, such as 1.0.2, for the new platform version.
    platformVersion :: Types.PlatformVersion,
    -- | The location of the platform definition archive in Amazon S3.
    platformDefinitionBundle :: Types.S3Location,
    -- | The name of the builder environment.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | The configuration option settings to apply to the builder environment.
    optionSettings :: Core.Maybe [Types.ConfigurationOptionSetting],
    -- | Specifies the tags applied to the new platform version.
    --
    -- Elastic Beanstalk applies these tags only to the platform version. Environments that you create using the platform version don't inherit the tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformVersion' value with any optional fields omitted.
mkCreatePlatformVersion ::
  -- | 'platformName'
  Types.PlatformName ->
  -- | 'platformVersion'
  Types.PlatformVersion ->
  -- | 'platformDefinitionBundle'
  Types.S3Location ->
  CreatePlatformVersion
mkCreatePlatformVersion
  platformName
  platformVersion
  platformDefinitionBundle =
    CreatePlatformVersion'
      { platformName,
        platformVersion,
        platformDefinitionBundle,
        environmentName = Core.Nothing,
        optionSettings = Core.Nothing,
        tags = Core.Nothing
      }

-- | The name of your custom platform.
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformName :: Lens.Lens' CreatePlatformVersion Types.PlatformName
cpvPlatformName = Lens.field @"platformName"
{-# DEPRECATED cpvPlatformName "Use generic-lens or generic-optics with 'platformName' instead." #-}

-- | The number, such as 1.0.2, for the new platform version.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformVersion :: Lens.Lens' CreatePlatformVersion Types.PlatformVersion
cpvPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED cpvPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The location of the platform definition archive in Amazon S3.
--
-- /Note:/ Consider using 'platformDefinitionBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformDefinitionBundle :: Lens.Lens' CreatePlatformVersion Types.S3Location
cpvPlatformDefinitionBundle = Lens.field @"platformDefinitionBundle"
{-# DEPRECATED cpvPlatformDefinitionBundle "Use generic-lens or generic-optics with 'platformDefinitionBundle' instead." #-}

-- | The name of the builder environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvEnvironmentName :: Lens.Lens' CreatePlatformVersion (Core.Maybe Types.EnvironmentName)
cpvEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED cpvEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The configuration option settings to apply to the builder environment.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvOptionSettings :: Lens.Lens' CreatePlatformVersion (Core.Maybe [Types.ConfigurationOptionSetting])
cpvOptionSettings = Lens.field @"optionSettings"
{-# DEPRECATED cpvOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | Specifies the tags applied to the new platform version.
--
-- Elastic Beanstalk applies these tags only to the platform version. Environments that you create using the platform version don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTags :: Lens.Lens' CreatePlatformVersion (Core.Maybe [Types.Tag])
cpvTags = Lens.field @"tags"
{-# DEPRECATED cpvTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreatePlatformVersion where
  type Rs CreatePlatformVersion = CreatePlatformVersionResponse
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
            ( Core.pure ("Action", "CreatePlatformVersion")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "PlatformName" platformName)
                Core.<> (Core.toQueryValue "PlatformVersion" platformVersion)
                Core.<> ( Core.toQueryValue
                            "PlatformDefinitionBundle"
                            platformDefinitionBundle
                        )
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> ( Core.toQueryValue
                            "OptionSettings"
                            (Core.toQueryList "member" Core.<$> optionSettings)
                        )
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreatePlatformVersionResult"
      ( \s h x ->
          CreatePlatformVersionResponse'
            Core.<$> (x Core..@? "Builder")
            Core.<*> (x Core..@? "PlatformSummary")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePlatformVersionResponse' smart constructor.
data CreatePlatformVersionResponse = CreatePlatformVersionResponse'
  { -- | The builder used to create the custom platform.
    builder :: Core.Maybe Types.Builder,
    -- | Detailed information about the new version of the custom platform.
    platformSummary :: Core.Maybe Types.PlatformSummary,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformVersionResponse' value with any optional fields omitted.
mkCreatePlatformVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePlatformVersionResponse
mkCreatePlatformVersionResponse responseStatus =
  CreatePlatformVersionResponse'
    { builder = Core.Nothing,
      platformSummary = Core.Nothing,
      responseStatus
    }

-- | The builder used to create the custom platform.
--
-- /Note:/ Consider using 'builder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsBuilder :: Lens.Lens' CreatePlatformVersionResponse (Core.Maybe Types.Builder)
cpvrrsBuilder = Lens.field @"builder"
{-# DEPRECATED cpvrrsBuilder "Use generic-lens or generic-optics with 'builder' instead." #-}

-- | Detailed information about the new version of the custom platform.
--
-- /Note:/ Consider using 'platformSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPlatformSummary :: Lens.Lens' CreatePlatformVersionResponse (Core.Maybe Types.PlatformSummary)
cpvrrsPlatformSummary = Lens.field @"platformSummary"
{-# DEPRECATED cpvrrsPlatformSummary "Use generic-lens or generic-optics with 'platformSummary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsResponseStatus :: Lens.Lens' CreatePlatformVersionResponse Core.Int
cpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
