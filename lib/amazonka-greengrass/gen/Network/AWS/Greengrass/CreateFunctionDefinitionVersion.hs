{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a Lambda function definition that has already been defined.
module Network.AWS.Greengrass.CreateFunctionDefinitionVersion
    (
    -- * Creating a request
      CreateFunctionDefinitionVersion (..)
    , mkCreateFunctionDefinitionVersion
    -- ** Request lenses
    , cfdvFunctionDefinitionId
    , cfdvAmznClientToken
    , cfdvDefaultConfig
    , cfdvFunctions

    -- * Destructuring the response
    , CreateFunctionDefinitionVersionResponse (..)
    , mkCreateFunctionDefinitionVersionResponse
    -- ** Response lenses
    , cfdvrrsArn
    , cfdvrrsCreationTimestamp
    , cfdvrrsId
    , cfdvrrsVersion
    , cfdvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Information needed to create a function definition version.
--
-- /See:/ 'mkCreateFunctionDefinitionVersion' smart constructor.
data CreateFunctionDefinitionVersion = CreateFunctionDefinitionVersion'
  { functionDefinitionId :: Core.Text
    -- ^ The ID of the Lambda function definition.
  , amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , defaultConfig :: Core.Maybe Types.FunctionDefaultConfig
    -- ^ The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
  , functions :: Core.Maybe [Types.Function]
    -- ^ A list of Lambda functions in this function definition version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFunctionDefinitionVersion' value with any optional fields omitted.
mkCreateFunctionDefinitionVersion
    :: Core.Text -- ^ 'functionDefinitionId'
    -> CreateFunctionDefinitionVersion
mkCreateFunctionDefinitionVersion functionDefinitionId
  = CreateFunctionDefinitionVersion'{functionDefinitionId,
                                     amznClientToken = Core.Nothing, defaultConfig = Core.Nothing,
                                     functions = Core.Nothing}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvFunctionDefinitionId :: Lens.Lens' CreateFunctionDefinitionVersion Core.Text
cfdvFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# INLINEABLE cfdvFunctionDefinitionId #-}
{-# DEPRECATED functionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead"  #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvAmznClientToken :: Lens.Lens' CreateFunctionDefinitionVersion (Core.Maybe Core.Text)
cfdvAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE cfdvAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
--
-- /Note:/ Consider using 'defaultConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvDefaultConfig :: Lens.Lens' CreateFunctionDefinitionVersion (Core.Maybe Types.FunctionDefaultConfig)
cfdvDefaultConfig = Lens.field @"defaultConfig"
{-# INLINEABLE cfdvDefaultConfig #-}
{-# DEPRECATED defaultConfig "Use generic-lens or generic-optics with 'defaultConfig' instead"  #-}

-- | A list of Lambda functions in this function definition version.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvFunctions :: Lens.Lens' CreateFunctionDefinitionVersion (Core.Maybe [Types.Function])
cfdvFunctions = Lens.field @"functions"
{-# INLINEABLE cfdvFunctions #-}
{-# DEPRECATED functions "Use generic-lens or generic-optics with 'functions' instead"  #-}

instance Core.ToQuery CreateFunctionDefinitionVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateFunctionDefinitionVersion where
        toHeaders CreateFunctionDefinitionVersion{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateFunctionDefinitionVersion where
        toJSON CreateFunctionDefinitionVersion{..}
          = Core.object
              (Core.catMaybes
                 [("DefaultConfig" Core..=) Core.<$> defaultConfig,
                  ("Functions" Core..=) Core.<$> functions])

instance Core.AWSRequest CreateFunctionDefinitionVersion where
        type Rs CreateFunctionDefinitionVersion =
             CreateFunctionDefinitionVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/greengrass/definition/functions/" Core.<>
                             Core.toText functionDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateFunctionDefinitionVersionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Id"
                     Core.<*> x Core..:? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateFunctionDefinitionVersionResponse' smart constructor.
data CreateFunctionDefinitionVersionResponse = CreateFunctionDefinitionVersionResponse'
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

-- | Creates a 'CreateFunctionDefinitionVersionResponse' value with any optional fields omitted.
mkCreateFunctionDefinitionVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateFunctionDefinitionVersionResponse
mkCreateFunctionDefinitionVersionResponse responseStatus
  = CreateFunctionDefinitionVersionResponse'{arn = Core.Nothing,
                                             creationTimestamp = Core.Nothing, id = Core.Nothing,
                                             version = Core.Nothing, responseStatus}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrrsArn :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
cfdvrrsArn = Lens.field @"arn"
{-# INLINEABLE cfdvrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrrsCreationTimestamp :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
cfdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE cfdvrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrrsId :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
cfdvrrsId = Lens.field @"id"
{-# INLINEABLE cfdvrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrrsVersion :: Lens.Lens' CreateFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
cfdvrrsVersion = Lens.field @"version"
{-# INLINEABLE cfdvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdvrrsResponseStatus :: Lens.Lens' CreateFunctionDefinitionVersionResponse Core.Int
cfdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cfdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
