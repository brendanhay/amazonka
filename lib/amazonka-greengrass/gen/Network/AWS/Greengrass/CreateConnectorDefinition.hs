{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connector definition. You may provide the initial version of the connector definition now or use ''CreateConnectorDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateConnectorDefinition
    (
    -- * Creating a request
      CreateConnectorDefinition (..)
    , mkCreateConnectorDefinition
    -- ** Request lenses
    , cAmznClientToken
    , cInitialVersion
    , cName
    , cTags

    -- * Destructuring the response
    , CreateConnectorDefinitionResponse (..)
    , mkCreateConnectorDefinitionResponse
    -- ** Response lenses
    , ccdrfrsArn
    , ccdrfrsCreationTimestamp
    , ccdrfrsId
    , ccdrfrsLastUpdatedTimestamp
    , ccdrfrsLatestVersion
    , ccdrfrsLatestVersionArn
    , ccdrfrsName
    , ccdrfrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateConnectorDefinition' smart constructor.
data CreateConnectorDefinition = CreateConnectorDefinition'
  { amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , initialVersion :: Core.Maybe Types.ConnectorDefinitionVersion
    -- ^ Information about the initial version of the connector definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the connector definition.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Tag(s) to add to the new resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConnectorDefinition' value with any optional fields omitted.
mkCreateConnectorDefinition
    :: CreateConnectorDefinition
mkCreateConnectorDefinition
  = CreateConnectorDefinition'{amznClientToken = Core.Nothing,
                               initialVersion = Core.Nothing, name = Core.Nothing,
                               tags = Core.Nothing}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAmznClientToken :: Lens.Lens' CreateConnectorDefinition (Core.Maybe Core.Text)
cAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE cAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | Information about the initial version of the connector definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInitialVersion :: Lens.Lens' CreateConnectorDefinition (Core.Maybe Types.ConnectorDefinitionVersion)
cInitialVersion = Lens.field @"initialVersion"
{-# INLINEABLE cInitialVersion #-}
{-# DEPRECATED initialVersion "Use generic-lens or generic-optics with 'initialVersion' instead"  #-}

-- | The name of the connector definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreateConnectorDefinition (Core.Maybe Core.Text)
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateConnectorDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateConnectorDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateConnectorDefinition where
        toHeaders CreateConnectorDefinition{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateConnectorDefinition where
        toJSON CreateConnectorDefinition{..}
          = Core.object
              (Core.catMaybes
                 [("InitialVersion" Core..=) Core.<$> initialVersion,
                  ("Name" Core..=) Core.<$> name, ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateConnectorDefinition where
        type Rs CreateConnectorDefinition =
             CreateConnectorDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/greengrass/definition/connectors",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateConnectorDefinitionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "LastUpdatedTimestamp"
                     Core.<*> x Core..:? "LatestVersion"
                     Core.<*> x Core..:? "LatestVersionArn"
                     Core.<*> x Core..:? "Name"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateConnectorDefinitionResponse' smart constructor.
data CreateConnectorDefinitionResponse = CreateConnectorDefinitionResponse'
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

-- | Creates a 'CreateConnectorDefinitionResponse' value with any optional fields omitted.
mkCreateConnectorDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateConnectorDefinitionResponse
mkCreateConnectorDefinitionResponse responseStatus
  = CreateConnectorDefinitionResponse'{arn = Core.Nothing,
                                       creationTimestamp = Core.Nothing, id = Core.Nothing,
                                       lastUpdatedTimestamp = Core.Nothing,
                                       latestVersion = Core.Nothing,
                                       latestVersionArn = Core.Nothing, name = Core.Nothing,
                                       responseStatus}

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsArn :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
ccdrfrsArn = Lens.field @"arn"
{-# INLINEABLE ccdrfrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsCreationTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
ccdrfrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE ccdrfrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsId :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
ccdrfrsId = Lens.field @"id"
{-# INLINEABLE ccdrfrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsLastUpdatedTimestamp :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
ccdrfrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# INLINEABLE ccdrfrsLastUpdatedTimestamp #-}
{-# DEPRECATED lastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead"  #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsLatestVersion :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
ccdrfrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE ccdrfrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsLatestVersionArn :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
ccdrfrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# INLINEABLE ccdrfrsLatestVersionArn #-}
{-# DEPRECATED latestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsName :: Lens.Lens' CreateConnectorDefinitionResponse (Core.Maybe Core.Text)
ccdrfrsName = Lens.field @"name"
{-# INLINEABLE ccdrfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrfrsResponseStatus :: Lens.Lens' CreateConnectorDefinitionResponse Core.Int
ccdrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccdrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
