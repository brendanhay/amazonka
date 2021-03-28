{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a connector definition which has already been defined.
module Network.AWS.Greengrass.CreateConnectorDefinitionVersion
    (
    -- * Creating a request
      CreateConnectorDefinitionVersion (..)
    , mkCreateConnectorDefinitionVersion
    -- ** Request lenses
    , ccdvConnectorDefinitionId
    , ccdvAmznClientToken
    , ccdvConnectors

    -- * Destructuring the response
    , CreateConnectorDefinitionVersionResponse (..)
    , mkCreateConnectorDefinitionVersionResponse
    -- ** Response lenses
    , ccdvrrsArn
    , ccdvrrsCreationTimestamp
    , ccdvrrsId
    , ccdvrrsVersion
    , ccdvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateConnectorDefinitionVersion' smart constructor.
data CreateConnectorDefinitionVersion = CreateConnectorDefinitionVersion'
  { connectorDefinitionId :: Core.Text
    -- ^ The ID of the connector definition.
  , amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , connectors :: Core.Maybe [Types.Connector]
    -- ^ A list of references to connectors in this version, with their corresponding configuration settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConnectorDefinitionVersion' value with any optional fields omitted.
mkCreateConnectorDefinitionVersion
    :: Core.Text -- ^ 'connectorDefinitionId'
    -> CreateConnectorDefinitionVersion
mkCreateConnectorDefinitionVersion connectorDefinitionId
  = CreateConnectorDefinitionVersion'{connectorDefinitionId,
                                      amznClientToken = Core.Nothing, connectors = Core.Nothing}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvConnectorDefinitionId :: Lens.Lens' CreateConnectorDefinitionVersion Core.Text
ccdvConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# INLINEABLE ccdvConnectorDefinitionId #-}
{-# DEPRECATED connectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead"  #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvAmznClientToken :: Lens.Lens' CreateConnectorDefinitionVersion (Core.Maybe Core.Text)
ccdvAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE ccdvAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | A list of references to connectors in this version, with their corresponding configuration settings.
--
-- /Note:/ Consider using 'connectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvConnectors :: Lens.Lens' CreateConnectorDefinitionVersion (Core.Maybe [Types.Connector])
ccdvConnectors = Lens.field @"connectors"
{-# INLINEABLE ccdvConnectors #-}
{-# DEPRECATED connectors "Use generic-lens or generic-optics with 'connectors' instead"  #-}

instance Core.ToQuery CreateConnectorDefinitionVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateConnectorDefinitionVersion where
        toHeaders CreateConnectorDefinitionVersion{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateConnectorDefinitionVersion where
        toJSON CreateConnectorDefinitionVersion{..}
          = Core.object
              (Core.catMaybes [("Connectors" Core..=) Core.<$> connectors])

instance Core.AWSRequest CreateConnectorDefinitionVersion where
        type Rs CreateConnectorDefinitionVersion =
             CreateConnectorDefinitionVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/greengrass/definition/connectors/" Core.<>
                             Core.toText connectorDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateConnectorDefinitionVersionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateConnectorDefinitionVersionResponse' smart constructor.
data CreateConnectorDefinitionVersionResponse = CreateConnectorDefinitionVersionResponse'
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

-- | Creates a 'CreateConnectorDefinitionVersionResponse' value with any optional fields omitted.
mkCreateConnectorDefinitionVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateConnectorDefinitionVersionResponse
mkCreateConnectorDefinitionVersionResponse responseStatus
  = CreateConnectorDefinitionVersionResponse'{arn = Core.Nothing,
                                              creationTimestamp = Core.Nothing, id = Core.Nothing,
                                              version = Core.Nothing, responseStatus}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrrsArn :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
ccdvrrsArn = Lens.field @"arn"
{-# INLINEABLE ccdvrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrrsCreationTimestamp :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
ccdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE ccdvrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrrsId :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
ccdvrrsId = Lens.field @"id"
{-# INLINEABLE ccdvrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrrsVersion :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
ccdvrrsVersion = Lens.field @"version"
{-# INLINEABLE ccdvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdvrrsResponseStatus :: Lens.Lens' CreateConnectorDefinitionVersionResponse Core.Int
ccdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
