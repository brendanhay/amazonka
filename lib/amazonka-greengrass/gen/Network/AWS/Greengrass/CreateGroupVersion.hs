{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateGroupVersion (..)
    , mkCreateGroupVersion
    -- ** Request lenses
    , cgvGroupId
    , cgvAmznClientToken
    , cgvConnectorDefinitionVersionArn
    , cgvCoreDefinitionVersionArn
    , cgvDeviceDefinitionVersionArn
    , cgvFunctionDefinitionVersionArn
    , cgvLoggerDefinitionVersionArn
    , cgvResourceDefinitionVersionArn
    , cgvSubscriptionDefinitionVersionArn

    -- * Destructuring the response
    , CreateGroupVersionResponse (..)
    , mkCreateGroupVersionResponse
    -- ** Response lenses
    , cgvrrsArn
    , cgvrrsCreationTimestamp
    , cgvrrsId
    , cgvrrsVersion
    , cgvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGroupVersion' smart constructor.
data CreateGroupVersion = CreateGroupVersion'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  , amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , connectorDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the connector definition version for this group.
  , coreDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the core definition version for this group.
  , deviceDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the device definition version for this group.
  , functionDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the function definition version for this group.
  , loggerDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the logger definition version for this group.
  , resourceDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the resource definition version for this group.
  , subscriptionDefinitionVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the subscription definition version for this group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupVersion' value with any optional fields omitted.
mkCreateGroupVersion
    :: Core.Text -- ^ 'groupId'
    -> CreateGroupVersion
mkCreateGroupVersion groupId
  = CreateGroupVersion'{groupId, amznClientToken = Core.Nothing,
                        connectorDefinitionVersionArn = Core.Nothing,
                        coreDefinitionVersionArn = Core.Nothing,
                        deviceDefinitionVersionArn = Core.Nothing,
                        functionDefinitionVersionArn = Core.Nothing,
                        loggerDefinitionVersionArn = Core.Nothing,
                        resourceDefinitionVersionArn = Core.Nothing,
                        subscriptionDefinitionVersionArn = Core.Nothing}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvGroupId :: Lens.Lens' CreateGroupVersion Core.Text
cgvGroupId = Lens.field @"groupId"
{-# INLINEABLE cgvGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvAmznClientToken :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE cgvAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | The ARN of the connector definition version for this group.
--
-- /Note:/ Consider using 'connectorDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvConnectorDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvConnectorDefinitionVersionArn = Lens.field @"connectorDefinitionVersionArn"
{-# INLINEABLE cgvConnectorDefinitionVersionArn #-}
{-# DEPRECATED connectorDefinitionVersionArn "Use generic-lens or generic-optics with 'connectorDefinitionVersionArn' instead"  #-}

-- | The ARN of the core definition version for this group.
--
-- /Note:/ Consider using 'coreDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvCoreDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvCoreDefinitionVersionArn = Lens.field @"coreDefinitionVersionArn"
{-# INLINEABLE cgvCoreDefinitionVersionArn #-}
{-# DEPRECATED coreDefinitionVersionArn "Use generic-lens or generic-optics with 'coreDefinitionVersionArn' instead"  #-}

-- | The ARN of the device definition version for this group.
--
-- /Note:/ Consider using 'deviceDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvDeviceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvDeviceDefinitionVersionArn = Lens.field @"deviceDefinitionVersionArn"
{-# INLINEABLE cgvDeviceDefinitionVersionArn #-}
{-# DEPRECATED deviceDefinitionVersionArn "Use generic-lens or generic-optics with 'deviceDefinitionVersionArn' instead"  #-}

-- | The ARN of the function definition version for this group.
--
-- /Note:/ Consider using 'functionDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvFunctionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvFunctionDefinitionVersionArn = Lens.field @"functionDefinitionVersionArn"
{-# INLINEABLE cgvFunctionDefinitionVersionArn #-}
{-# DEPRECATED functionDefinitionVersionArn "Use generic-lens or generic-optics with 'functionDefinitionVersionArn' instead"  #-}

-- | The ARN of the logger definition version for this group.
--
-- /Note:/ Consider using 'loggerDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvLoggerDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvLoggerDefinitionVersionArn = Lens.field @"loggerDefinitionVersionArn"
{-# INLINEABLE cgvLoggerDefinitionVersionArn #-}
{-# DEPRECATED loggerDefinitionVersionArn "Use generic-lens or generic-optics with 'loggerDefinitionVersionArn' instead"  #-}

-- | The ARN of the resource definition version for this group.
--
-- /Note:/ Consider using 'resourceDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvResourceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvResourceDefinitionVersionArn = Lens.field @"resourceDefinitionVersionArn"
{-# INLINEABLE cgvResourceDefinitionVersionArn #-}
{-# DEPRECATED resourceDefinitionVersionArn "Use generic-lens or generic-optics with 'resourceDefinitionVersionArn' instead"  #-}

-- | The ARN of the subscription definition version for this group.
--
-- /Note:/ Consider using 'subscriptionDefinitionVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvSubscriptionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
cgvSubscriptionDefinitionVersionArn = Lens.field @"subscriptionDefinitionVersionArn"
{-# INLINEABLE cgvSubscriptionDefinitionVersionArn #-}
{-# DEPRECATED subscriptionDefinitionVersionArn "Use generic-lens or generic-optics with 'subscriptionDefinitionVersionArn' instead"  #-}

instance Core.ToQuery CreateGroupVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGroupVersion where
        toHeaders CreateGroupVersion{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateGroupVersion where
        toJSON CreateGroupVersion{..}
          = Core.object
              (Core.catMaybes
                 [("ConnectorDefinitionVersionArn" Core..=) Core.<$>
                    connectorDefinitionVersionArn,
                  ("CoreDefinitionVersionArn" Core..=) Core.<$>
                    coreDefinitionVersionArn,
                  ("DeviceDefinitionVersionArn" Core..=) Core.<$>
                    deviceDefinitionVersionArn,
                  ("FunctionDefinitionVersionArn" Core..=) Core.<$>
                    functionDefinitionVersionArn,
                  ("LoggerDefinitionVersionArn" Core..=) Core.<$>
                    loggerDefinitionVersionArn,
                  ("ResourceDefinitionVersionArn" Core..=) Core.<$>
                    resourceDefinitionVersionArn,
                  ("SubscriptionDefinitionVersionArn" Core..=) Core.<$>
                    subscriptionDefinitionVersionArn])

instance Core.AWSRequest CreateGroupVersion where
        type Rs CreateGroupVersion = CreateGroupVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<>
                             "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGroupVersionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGroupVersionResponse' smart constructor.
data CreateGroupVersionResponse = CreateGroupVersionResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the version.
  , creationTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the version was created.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the parent definition that the version is associated with.
  , version :: Core.Maybe Core.Text
    -- ^ The ID of the version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupVersionResponse' value with any optional fields omitted.
mkCreateGroupVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGroupVersionResponse
mkCreateGroupVersionResponse responseStatus
  = CreateGroupVersionResponse'{arn = Core.Nothing,
                                creationTimestamp = Core.Nothing, id = Core.Nothing,
                                version = Core.Nothing, responseStatus}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsArn :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsArn = Lens.field @"arn"
{-# INLINEABLE cgvrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsCreationTimestamp :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE cgvrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsId :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsId = Lens.field @"id"
{-# INLINEABLE cgvrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsVersion :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
cgvrrsVersion = Lens.field @"version"
{-# INLINEABLE cgvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgvrrsResponseStatus :: Lens.Lens' CreateGroupVersionResponse Core.Int
cgvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
