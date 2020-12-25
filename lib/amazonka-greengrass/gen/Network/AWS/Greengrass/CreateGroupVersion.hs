{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateGroupVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a group which has already been defined.
module Network.AWS.Greengrass.CreateGroupVersion
  ( -- * Creating a request
    CreateGroupVersion (..),
    mkCreateGroupVersion,

    -- ** Request lenses
    cgvGroupId,
    cgvAmznClientToken,
    cgvConnectorDefinitionVersionArn,
    cgvCoreDefinitionVersionArn,
    cgvDeviceDefinitionVersionArn,
    cgvFunctionDefinitionVersionArn,
    cgvLoggerDefinitionVersionArn,
    cgvResourceDefinitionVersionArn,
    cgvSubscriptionDefinitionVersionArn,

    -- * Destructuring the response
    CreateGroupVersionResponse (..),
    mkCreateGroupVersionResponse,

    -- ** Response lenses
    cgvrrsArn,
    cgvrrsCreationTimestamp,
    cgvrrsId,
    cgvrrsVersion,
    cgvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGroupVersion' smart constructor.
data CreateGroupVersion = CreateGroupVersion'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the core definition version for this group.
    coreDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionArn :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupVersion' value with any optional fields omitted.
mkCreateGroupVersion ::
  -- | 'groupId'
  Core.Text ->
  CreateGroupVersion
mkCreateGroupVersion groupId =
  CreateGroupVersion'
    { groupId,
      amznClientToken = Core.Nothing,
      connectorDefinitionVersionArn = Core.Nothing,
      coreDefinitionVersionArn = Core.Nothing,
      deviceDefinitionVersionArn = Core.Nothing,
      functionDefinitionVersionArn = Core.Nothing,
      loggerDefinitionVersionArn = Core.Nothing,
      resourceDefinitionVersionArn = Core.Nothing,
      subscriptionDefinitionVersionArn = Core.Nothing
    }

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvGroupId :: Lens.Lens' CreateGroupVersion Core.Text
cgvGroupId = Lens.field @"groupId"
{-# DEPRECATED cgvGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvAmznClientToken :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED cgvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | The ARN of the connector definition version for this group.
--
-- /Note:/ Consider using 'connectorDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvConnectorDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvConnectorDefinitionVersionArn = Lens.field @"connectorDefinitionVersionArn"
{-# DEPRECATED cgvConnectorDefinitionVersionArn "Use generic-lens or generic-optics with 'connectorDefinitionVersionArn' instead." #-}

-- | The ARN of the core definition version for this group.
--
-- /Note:/ Consider using 'coreDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvCoreDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvCoreDefinitionVersionArn = Lens.field @"coreDefinitionVersionArn"
{-# DEPRECATED cgvCoreDefinitionVersionArn "Use generic-lens or generic-optics with 'coreDefinitionVersionArn' instead." #-}

-- | The ARN of the device definition version for this group.
--
-- /Note:/ Consider using 'deviceDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvDeviceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvDeviceDefinitionVersionArn = Lens.field @"deviceDefinitionVersionArn"
{-# DEPRECATED cgvDeviceDefinitionVersionArn "Use generic-lens or generic-optics with 'deviceDefinitionVersionArn' instead." #-}

-- | The ARN of the function definition version for this group.
--
-- /Note:/ Consider using 'functionDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvFunctionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvFunctionDefinitionVersionArn = Lens.field @"functionDefinitionVersionArn"
{-# DEPRECATED cgvFunctionDefinitionVersionArn "Use generic-lens or generic-optics with 'functionDefinitionVersionArn' instead." #-}

-- | The ARN of the logger definition version for this group.
--
-- /Note:/ Consider using 'loggerDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvLoggerDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvLoggerDefinitionVersionArn = Lens.field @"loggerDefinitionVersionArn"
{-# DEPRECATED cgvLoggerDefinitionVersionArn "Use generic-lens or generic-optics with 'loggerDefinitionVersionArn' instead." #-}

-- | The ARN of the resource definition version for this group.
--
-- /Note:/ Consider using 'resourceDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvResourceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvResourceDefinitionVersionArn = Lens.field @"resourceDefinitionVersionArn"
{-# DEPRECATED cgvResourceDefinitionVersionArn "Use generic-lens or generic-optics with 'resourceDefinitionVersionArn' instead." #-}

-- | The ARN of the subscription definition version for this group.
--
-- /Note:/ Consider using 'subscriptionDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvSubscriptionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvSubscriptionDefinitionVersionArn = Lens.field @"subscriptionDefinitionVersionArn"
{-# DEPRECATED cgvSubscriptionDefinitionVersionArn "Use generic-lens or generic-optics with 'subscriptionDefinitionVersionArn' instead." #-}

instance Core.FromJSON CreateGroupVersion where
  toJSON CreateGroupVersion {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectorDefinitionVersionArn" Core..=)
              Core.<$> connectorDefinitionVersionArn,
            ("CoreDefinitionVersionArn" Core..=)
              Core.<$> coreDefinitionVersionArn,
            ("DeviceDefinitionVersionArn" Core..=)
              Core.<$> deviceDefinitionVersionArn,
            ("FunctionDefinitionVersionArn" Core..=)
              Core.<$> functionDefinitionVersionArn,
            ("LoggerDefinitionVersionArn" Core..=)
              Core.<$> loggerDefinitionVersionArn,
            ("ResourceDefinitionVersionArn" Core..=)
              Core.<$> resourceDefinitionVersionArn,
            ("SubscriptionDefinitionVersionArn" Core..=)
              Core.<$> subscriptionDefinitionVersionArn
          ]
      )

instance Core.AWSRequest CreateGroupVersion where
  type Rs CreateGroupVersion = CreateGroupVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/groups/" Core.<> (Core.toText groupId)
                Core.<> ("/versions")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGroupVersionResponse' smart constructor.
data CreateGroupVersionResponse = CreateGroupVersionResponse'
  { -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupVersionResponse' value with any optional fields omitted.
mkCreateGroupVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGroupVersionResponse
mkCreateGroupVersionResponse responseStatus =
  CreateGroupVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsArn :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsArn = Lens.field @"arn"
{-# DEPRECATED cgvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsCreationTimestamp :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED cgvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsId :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsId = Lens.field @"id"
{-# DEPRECATED cgvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsVersion :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsVersion = Lens.field @"version"
{-# DEPRECATED cgvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsResponseStatus :: Lens.Lens' CreateGroupVersionResponse Core.Int
cgvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
