{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core definition. You may provide the initial version of the core definition now or use ''CreateCoreDefinitionVersion'' at a later time. Greengrass groups must each contain exactly one Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinition
    (
    -- * Creating a request
      CreateCoreDefinition (..)
    , mkCreateCoreDefinition
    -- ** Request lenses
    , ccdAmznClientToken
    , ccdInitialVersion
    , ccdName
    , ccdTags

    -- * Destructuring the response
    , CreateCoreDefinitionResponse (..)
    , mkCreateCoreDefinitionResponse
    -- ** Response lenses
    , ccdrrsArn
    , ccdrrsCreationTimestamp
    , ccdrrsId
    , ccdrrsLastUpdatedTimestamp
    , ccdrrsLatestVersion
    , ccdrrsLatestVersionArn
    , ccdrrsName
    , ccdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Information needed to create a core definition.
--
-- /See:/ 'mkCreateCoreDefinition' smart constructor.
data CreateCoreDefinition = CreateCoreDefinition'
  { amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , initialVersion :: Core.Maybe Types.CoreDefinitionVersion
    -- ^ Information about the initial version of the core definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the core definition.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Tag(s) to add to the new resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCoreDefinition' value with any optional fields omitted.
mkCreateCoreDefinition
    :: CreateCoreDefinition
mkCreateCoreDefinition
  = CreateCoreDefinition'{amznClientToken = Core.Nothing,
                          initialVersion = Core.Nothing, name = Core.Nothing,
                          tags = Core.Nothing}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdAmznClientToken :: Lens.Lens' CreateCoreDefinition (Core.Maybe Core.Text)
ccdAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE ccdAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | Information about the initial version of the core definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdInitialVersion :: Lens.Lens' CreateCoreDefinition (Core.Maybe Types.CoreDefinitionVersion)
ccdInitialVersion = Lens.field @"initialVersion"
{-# INLINEABLE ccdInitialVersion #-}
{-# DEPRECATED initialVersion "Use generic-lens or generic-optics with 'initialVersion' instead"  #-}

-- | The name of the core definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdName :: Lens.Lens' CreateCoreDefinition (Core.Maybe Core.Text)
ccdName = Lens.field @"name"
{-# INLINEABLE ccdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdTags :: Lens.Lens' CreateCoreDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
ccdTags = Lens.field @"tags"
{-# INLINEABLE ccdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCoreDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCoreDefinition where
        toHeaders CreateCoreDefinition{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCoreDefinition where
        toJSON CreateCoreDefinition{..}
          = Core.object
              (Core.catMaybes
                 [("InitialVersion" Core..=) Core.<$> initialVersion,
                  ("Name" Core..=) Core.<$> name, ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCoreDefinition where
        type Rs CreateCoreDefinition = CreateCoreDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/greengrass/definition/cores",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCoreDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCoreDefinitionResponse' smart constructor.
data CreateCoreDefinitionResponse = CreateCoreDefinitionResponse'
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

-- | Creates a 'CreateCoreDefinitionResponse' value with any optional fields omitted.
mkCreateCoreDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCoreDefinitionResponse
mkCreateCoreDefinitionResponse responseStatus
  = CreateCoreDefinitionResponse'{arn = Core.Nothing,
                                  creationTimestamp = Core.Nothing, id = Core.Nothing,
                                  lastUpdatedTimestamp = Core.Nothing, latestVersion = Core.Nothing,
                                  latestVersionArn = Core.Nothing, name = Core.Nothing,
                                  responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsArn :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsArn = Lens.field @"arn"
{-# INLINEABLE ccdrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsCreationTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE ccdrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsId :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsId = Lens.field @"id"
{-# INLINEABLE ccdrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsLastUpdatedTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE ccdrrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsLatestVersion :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE ccdrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsLatestVersionArn :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE ccdrrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsName :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsName = Lens.field @"name"
{-# INLINEABLE ccdrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsResponseStatus :: Lens.Lens' CreateCoreDefinitionResponse Core.Int
ccdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
