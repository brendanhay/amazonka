{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.CreateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version).
module Network.AWS.MQ.CreateConfiguration
  ( -- * Creating a request
    CreateConfiguration (..),
    mkCreateConfiguration,

    -- ** Request lenses
    ccAuthenticationStrategy,
    ccEngineType,
    ccEngineVersion,
    ccName,
    ccTags,

    -- * Destructuring the response
    CreateConfigurationResponse (..),
    mkCreateConfigurationResponse,

    -- ** Response lenses
    ccrrsArn,
    ccrrsAuthenticationStrategy,
    ccrrsCreated,
    ccrrsId,
    ccrrsLatestRevision,
    ccrrsName,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new configuration for the specified configuration name. Amazon MQ uses the default configuration (the engine type and version).
--
-- /See:/ 'mkCreateConfiguration' smart constructor.
data CreateConfiguration = CreateConfiguration'
  { -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy,
    -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
    engineType :: Core.Maybe Types.EngineType,
    -- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
    name :: Core.Maybe Core.Text,
    -- | Create tags when creating the configuration.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfiguration' value with any optional fields omitted.
mkCreateConfiguration ::
  CreateConfiguration
mkCreateConfiguration =
  CreateConfiguration'
    { authenticationStrategy = Core.Nothing,
      engineType = Core.Nothing,
      engineVersion = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthenticationStrategy :: Lens.Lens' CreateConfiguration (Core.Maybe Types.AuthenticationStrategy)
ccAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# DEPRECATED ccAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngineType :: Lens.Lens' CreateConfiguration (Core.Maybe Types.EngineType)
ccEngineType = Lens.field @"engineType"
{-# DEPRECATED ccEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngineVersion :: Lens.Lens' CreateConfiguration (Core.Maybe Core.Text)
ccEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED ccEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccName :: Lens.Lens' CreateConfiguration (Core.Maybe Core.Text)
ccName = Lens.field @"name"
{-# DEPRECATED ccName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Create tags when creating the configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
ccTags = Lens.field @"tags"
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateConfiguration where
  toJSON CreateConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("authenticationStrategy" Core..=)
              Core.<$> authenticationStrategy,
            ("engineType" Core..=) Core.<$> engineType,
            ("engineVersion" Core..=) Core.<$> engineVersion,
            ("name" Core..=) Core.<$> name,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateConfiguration where
  type Rs CreateConfiguration = CreateConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/configurations",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfigurationResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "authenticationStrategy")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "latestRevision")
            Core.<*> (x Core..:? "name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateConfigurationResponse' smart constructor.
data CreateConfigurationResponse = CreateConfigurationResponse'
  { -- | Required. The Amazon Resource Name (ARN) of the configuration.
    arn :: Core.Maybe Core.Text,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy,
    -- | Required. The date and time of the configuration.
    created :: Core.Maybe Core.UTCTime,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Core.Maybe Core.Text,
    -- | The latest revision of the configuration.
    latestRevision :: Core.Maybe Types.ConfigurationRevision,
    -- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
    name :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateConfigurationResponse' value with any optional fields omitted.
mkCreateConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateConfigurationResponse
mkCreateConfigurationResponse responseStatus =
  CreateConfigurationResponse'
    { arn = Core.Nothing,
      authenticationStrategy = Core.Nothing,
      created = Core.Nothing,
      id = Core.Nothing,
      latestRevision = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | Required. The Amazon Resource Name (ARN) of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsArn :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.Text)
ccrrsArn = Lens.field @"arn"
{-# DEPRECATED ccrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsAuthenticationStrategy :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Types.AuthenticationStrategy)
ccrrsAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# DEPRECATED ccrrsAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. The date and time of the configuration.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCreated :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.UTCTime)
ccrrsCreated = Lens.field @"created"
{-# DEPRECATED ccrrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsId :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.Text)
ccrrsId = Lens.field @"id"
{-# DEPRECATED ccrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsLatestRevision :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Types.ConfigurationRevision)
ccrrsLatestRevision = Lens.field @"latestRevision"
{-# DEPRECATED ccrrsLatestRevision "Use generic-lens or generic-optics with 'latestRevision' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsName :: Lens.Lens' CreateConfigurationResponse (Core.Maybe Core.Text)
ccrrsName = Lens.field @"name"
{-# DEPRECATED ccrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateConfigurationResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
