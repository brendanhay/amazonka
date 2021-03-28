{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function definition which contains a list of Lambda functions and their configurations to be used in a group. You can create an initial version of the definition by providing a list of Lambda functions and their configurations now, or use ''CreateFunctionDefinitionVersion'' later.
module Network.AWS.Greengrass.CreateFunctionDefinition
    (
    -- * Creating a request
      CreateFunctionDefinition (..)
    , mkCreateFunctionDefinition
    -- ** Request lenses
    , cfdAmznClientToken
    , cfdInitialVersion
    , cfdName
    , cfdTags

    -- * Destructuring the response
    , CreateFunctionDefinitionResponse (..)
    , mkCreateFunctionDefinitionResponse
    -- ** Response lenses
    , cfdrrsArn
    , cfdrrsCreationTimestamp
    , cfdrrsId
    , cfdrrsLastUpdatedTimestamp
    , cfdrrsLatestVersion
    , cfdrrsLatestVersionArn
    , cfdrrsName
    , cfdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFunctionDefinition' smart constructor.
data CreateFunctionDefinition = CreateFunctionDefinition'
  { amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , initialVersion :: Core.Maybe Types.FunctionDefinitionVersion
    -- ^ Information about the initial version of the function definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the function definition.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Tag(s) to add to the new resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFunctionDefinition' value with any optional fields omitted.
mkCreateFunctionDefinition
    :: CreateFunctionDefinition
mkCreateFunctionDefinition
  = CreateFunctionDefinition'{amznClientToken = Core.Nothing,
                              initialVersion = Core.Nothing, name = Core.Nothing,
                              tags = Core.Nothing}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdAmznClientToken :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Core.Text)
cfdAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE cfdAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | Information about the initial version of the function definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdInitialVersion :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Types.FunctionDefinitionVersion)
cfdInitialVersion = Lens.field @"initialVersion"
{-# INLINEABLE cfdInitialVersion #-}
{-# DEPRECATED initialVersion "Use generic-lens or generic-optics with 'initialVersion' instead"  #-}

-- | The name of the function definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdName :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Core.Text)
cfdName = Lens.field @"name"
{-# INLINEABLE cfdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdTags :: Lens.Lens' CreateFunctionDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
cfdTags = Lens.field @"tags"
{-# INLINEABLE cfdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateFunctionDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateFunctionDefinition where
        toHeaders CreateFunctionDefinition{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateFunctionDefinition where
        toJSON CreateFunctionDefinition{..}
          = Core.object
              (Core.catMaybes
                 [("InitialVersion" Core..=) Core.<$> initialVersion,
                  ("Name" Core..=) Core.<$> name, ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateFunctionDefinition where
        type Rs CreateFunctionDefinition = CreateFunctionDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/greengrass/definition/functions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateFunctionDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateFunctionDefinitionResponse' smart constructor.
data CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse'
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

-- | Creates a 'CreateFunctionDefinitionResponse' value with any optional fields omitted.
mkCreateFunctionDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateFunctionDefinitionResponse
mkCreateFunctionDefinitionResponse responseStatus
  = CreateFunctionDefinitionResponse'{arn = Core.Nothing,
                                      creationTimestamp = Core.Nothing, id = Core.Nothing,
                                      lastUpdatedTimestamp = Core.Nothing,
                                      latestVersion = Core.Nothing, latestVersionArn = Core.Nothing,
                                      name = Core.Nothing, responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsArn :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsArn = Lens.field @"arn"
{-# INLINEABLE cfdrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsCreationTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE cfdrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsId :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsId = Lens.field @"id"
{-# INLINEABLE cfdrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsLastUpdatedTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE cfdrrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsLatestVersion :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE cfdrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsLatestVersionArn :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE cfdrrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsName :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsName = Lens.field @"name"
{-# INLINEABLE cfdrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsResponseStatus :: Lens.Lens' CreateFunctionDefinitionResponse Core.Int
cfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
