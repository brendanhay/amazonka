{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified configuration.
module Network.AWS.MQ.DescribeConfiguration
  ( -- * Creating a request
    DescribeConfiguration (..),
    mkDescribeConfiguration,

    -- ** Request lenses
    dcConfigurationId,

    -- * Destructuring the response
    DescribeConfigurationResponse (..),
    mkDescribeConfigurationResponse,

    -- ** Response lenses
    dcrrsArn,
    dcrrsAuthenticationStrategy,
    dcrrsCreated,
    dcrrsDescription,
    dcrrsEngineType,
    dcrrsEngineVersion,
    dcrrsId,
    dcrrsLatestRevision,
    dcrrsName,
    dcrrsTags,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConfiguration' smart constructor.
newtype DescribeConfiguration = DescribeConfiguration'
  { -- | The unique ID that Amazon MQ generates for the configuration.
    configurationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfiguration' value with any optional fields omitted.
mkDescribeConfiguration ::
  -- | 'configurationId'
  Core.Text ->
  DescribeConfiguration
mkDescribeConfiguration configurationId =
  DescribeConfiguration' {configurationId}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConfigurationId :: Lens.Lens' DescribeConfiguration Core.Text
dcConfigurationId = Lens.field @"configurationId"
{-# DEPRECATED dcConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Core.AWSRequest DescribeConfiguration where
  type Rs DescribeConfiguration = DescribeConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/v1/configurations/" Core.<> (Core.toText configurationId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "authenticationStrategy")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "engineType")
            Core.<*> (x Core..:? "engineVersion")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "latestRevision")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeConfigurationResponse' smart constructor.
data DescribeConfigurationResponse = DescribeConfigurationResponse'
  { -- | Required. The ARN of the configuration.
    arn :: Core.Maybe Core.Text,
    -- | The authentication strategy associated with the configuration.
    authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy,
    -- | Required. The date and time of the configuration revision.
    created :: Core.Maybe Core.UTCTime,
    -- | Required. The description of the configuration.
    description :: Core.Maybe Core.Text,
    -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
    engineType :: Core.Maybe Types.EngineType,
    -- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Core.Maybe Core.Text,
    -- | Required. The latest revision of the configuration.
    latestRevision :: Core.Maybe Types.ConfigurationRevision,
    -- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
    name :: Core.Maybe Core.Text,
    -- | The list of all tags associated with this configuration.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeConfigurationResponse' value with any optional fields omitted.
mkDescribeConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConfigurationResponse
mkDescribeConfigurationResponse responseStatus =
  DescribeConfigurationResponse'
    { arn = Core.Nothing,
      authenticationStrategy = Core.Nothing,
      created = Core.Nothing,
      description = Core.Nothing,
      engineType = Core.Nothing,
      engineVersion = Core.Nothing,
      id = Core.Nothing,
      latestRevision = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Required. The ARN of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsArn :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
dcrrsArn = Lens.field @"arn"
{-# DEPRECATED dcrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The authentication strategy associated with the configuration.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsAuthenticationStrategy :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Types.AuthenticationStrategy)
dcrrsAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# DEPRECATED dcrrsAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | Required. The date and time of the configuration revision.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCreated :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.UTCTime)
dcrrsCreated = Lens.field @"created"
{-# DEPRECATED dcrrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Required. The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsDescription :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
dcrrsDescription = Lens.field @"description"
{-# DEPRECATED dcrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsEngineType :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Types.EngineType)
dcrrsEngineType = Lens.field @"engineType"
{-# DEPRECATED dcrrsEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | Required. The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsEngineVersion :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
dcrrsEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dcrrsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsId :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
dcrrsId = Lens.field @"id"
{-# DEPRECATED dcrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Required. The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsLatestRevision :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Types.ConfigurationRevision)
dcrrsLatestRevision = Lens.field @"latestRevision"
{-# DEPRECATED dcrrsLatestRevision "Use generic-lens or generic-optics with 'latestRevision' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsName :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe Core.Text)
dcrrsName = Lens.field @"name"
{-# DEPRECATED dcrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of all tags associated with this configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsTags :: Lens.Lens' DescribeConfigurationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dcrrsTags = Lens.field @"tags"
{-# DEPRECATED dcrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeConfigurationResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
