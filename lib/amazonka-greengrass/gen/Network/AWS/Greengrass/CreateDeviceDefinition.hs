{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device definition. You may provide the initial version of the device definition now or use ''CreateDeviceDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateDeviceDefinition
    (
    -- * Creating a request
      CreateDeviceDefinition (..)
    , mkCreateDeviceDefinition
    -- ** Request lenses
    , cddAmznClientToken
    , cddInitialVersion
    , cddName
    , cddTags

    -- * Destructuring the response
    , CreateDeviceDefinitionResponse (..)
    , mkCreateDeviceDefinitionResponse
    -- ** Response lenses
    , cddrrsArn
    , cddrrsCreationTimestamp
    , cddrrsId
    , cddrrsLastUpdatedTimestamp
    , cddrrsLatestVersion
    , cddrrsLatestVersionArn
    , cddrrsName
    , cddrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDeviceDefinition' smart constructor.
data CreateDeviceDefinition = CreateDeviceDefinition'
  { amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , initialVersion :: Core.Maybe Types.DeviceDefinitionVersion
    -- ^ Information about the initial version of the device definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the device definition.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Tag(s) to add to the new resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeviceDefinition' value with any optional fields omitted.
mkCreateDeviceDefinition
    :: CreateDeviceDefinition
mkCreateDeviceDefinition
  = CreateDeviceDefinition'{amznClientToken = Core.Nothing,
                            initialVersion = Core.Nothing, name = Core.Nothing,
                            tags = Core.Nothing}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddAmznClientToken :: Lens.Lens' CreateDeviceDefinition (Core.Maybe Core.Text)
cddAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE cddAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | Information about the initial version of the device definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddInitialVersion :: Lens.Lens' CreateDeviceDefinition (Core.Maybe Types.DeviceDefinitionVersion)
cddInitialVersion = Lens.field @"initialVersion"
{-# INLINEABLE cddInitialVersion #-}
{-# DEPRECATED initialVersion "Use generic-lens or generic-optics with 'initialVersion' instead"  #-}

-- | The name of the device definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddName :: Lens.Lens' CreateDeviceDefinition (Core.Maybe Core.Text)
cddName = Lens.field @"name"
{-# INLINEABLE cddName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddTags :: Lens.Lens' CreateDeviceDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
cddTags = Lens.field @"tags"
{-# INLINEABLE cddTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDeviceDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDeviceDefinition where
        toHeaders CreateDeviceDefinition{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDeviceDefinition where
        toJSON CreateDeviceDefinition{..}
          = Core.object
              (Core.catMaybes
                 [("InitialVersion" Core..=) Core.<$> initialVersion,
                  ("Name" Core..=) Core.<$> name, ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDeviceDefinition where
        type Rs CreateDeviceDefinition = CreateDeviceDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/greengrass/definition/devices",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDeviceDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDeviceDefinitionResponse' smart constructor.
data CreateDeviceDefinitionResponse = CreateDeviceDefinitionResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the definition.
  , creationTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the definition was created.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the definition.
  , lastUpdatedTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the definition was last updated.
  , latestVersion :: Core.Maybe Core.Text
    -- ^ The ID of the latest version associated with the definition.
  , latestVersionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the latest version associated with the definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeviceDefinitionResponse' value with any optional fields omitted.
mkCreateDeviceDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDeviceDefinitionResponse
mkCreateDeviceDefinitionResponse responseStatus
  = CreateDeviceDefinitionResponse'{arn = Core.Nothing,
                                    creationTimestamp = Core.Nothing, id = Core.Nothing,
                                    lastUpdatedTimestamp = Core.Nothing,
                                    latestVersion = Core.Nothing, latestVersionArn = Core.Nothing,
                                    name = Core.Nothing, responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsArn :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
cddrrsArn = Lens.field @"arn"
{-# INLINEABLE cddrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsCreationTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
cddrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE cddrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsId :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
cddrrsId = Lens.field @"id"
{-# INLINEABLE cddrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsLastUpdatedTimestamp :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
cddrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE cddrrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsLatestVersion :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
cddrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE cddrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsLatestVersionArn :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
cddrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE cddrrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsName :: Lens.Lens' CreateDeviceDefinitionResponse (Core.Maybe Core.Text)
cddrrsName = Lens.field @"name"
{-# INLINEABLE cddrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddrrsResponseStatus :: Lens.Lens' CreateDeviceDefinitionResponse Core.Int
cddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
