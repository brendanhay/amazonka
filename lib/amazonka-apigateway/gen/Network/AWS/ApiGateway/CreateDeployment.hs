{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Deployment' resource, which makes a specified 'RestApi' callable over the internet.
module Network.AWS.ApiGateway.CreateDeployment
    (
    -- * Creating a request
      CreateDeployment (..)
    , mkCreateDeployment
    -- ** Request lenses
    , cdRestApiId
    , cdCacheClusterEnabled
    , cdCacheClusterSize
    , cdCanarySettings
    , cdDescription
    , cdStageDescription
    , cdStageName
    , cdTracingEnabled
    , cdVariables

     -- * Destructuring the response
    , Types.Deployment (..)
    , Types.mkDeployment
    -- ** Response lenses
    , Types.dApiSummary
    , Types.dCreatedDate
    , Types.dDescription
    , Types.dId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to create a 'Deployment' resource.
--
-- /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , cacheClusterEnabled :: Core.Maybe Core.Bool
    -- ^ Enables a cache cluster for the 'Stage' resource specified in the input.
  , cacheClusterSize :: Core.Maybe Types.CacheClusterSize
    -- ^ Specifies the cache cluster size for the 'Stage' resource specified in the input, if a cache cluster is enabled.
  , canarySettings :: Core.Maybe Types.DeploymentCanarySettings
    -- ^ The input configuration for the canary deployment when the deployment is a canary release deployment. 
  , description :: Core.Maybe Core.Text
    -- ^ The description for the 'Deployment' resource to create.
  , stageDescription :: Core.Maybe Core.Text
    -- ^ The description of the 'Stage' resource for the 'Deployment' resource to create.
  , stageName :: Core.Maybe Core.Text
    -- ^ The name of the 'Stage' resource for the 'Deployment' resource to create.
  , tracingEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether active tracing with X-ray is enabled for the 'Stage' .
  , variables :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A map that defines the stage variables for the 'Stage' resource that is associated with the new deployment. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDeployment' value with any optional fields omitted.
mkCreateDeployment
    :: Core.Text -- ^ 'restApiId'
    -> CreateDeployment
mkCreateDeployment restApiId
  = CreateDeployment'{restApiId, cacheClusterEnabled = Core.Nothing,
                      cacheClusterSize = Core.Nothing, canarySettings = Core.Nothing,
                      description = Core.Nothing, stageDescription = Core.Nothing,
                      stageName = Core.Nothing, tracingEnabled = Core.Nothing,
                      variables = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRestApiId :: Lens.Lens' CreateDeployment Core.Text
cdRestApiId = Lens.field @"restApiId"
{-# INLINEABLE cdRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | Enables a cache cluster for the 'Stage' resource specified in the input.
--
-- /Note:/ Consider using 'cacheClusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCacheClusterEnabled :: Lens.Lens' CreateDeployment (Core.Maybe Core.Bool)
cdCacheClusterEnabled = Lens.field @"cacheClusterEnabled"
{-# INLINEABLE cdCacheClusterEnabled #-}
{-# DEPRECATED cacheClusterEnabled "Use generic-lens or generic-optics with 'cacheClusterEnabled' instead"  #-}

-- | Specifies the cache cluster size for the 'Stage' resource specified in the input, if a cache cluster is enabled.
--
-- /Note:/ Consider using 'cacheClusterSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCacheClusterSize :: Lens.Lens' CreateDeployment (Core.Maybe Types.CacheClusterSize)
cdCacheClusterSize = Lens.field @"cacheClusterSize"
{-# INLINEABLE cdCacheClusterSize #-}
{-# DEPRECATED cacheClusterSize "Use generic-lens or generic-optics with 'cacheClusterSize' instead"  #-}

-- | The input configuration for the canary deployment when the deployment is a canary release deployment. 
--
-- /Note:/ Consider using 'canarySettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCanarySettings :: Lens.Lens' CreateDeployment (Core.Maybe Types.DeploymentCanarySettings)
cdCanarySettings = Lens.field @"canarySettings"
{-# INLINEABLE cdCanarySettings #-}
{-# DEPRECATED canarySettings "Use generic-lens or generic-optics with 'canarySettings' instead"  #-}

-- | The description for the 'Deployment' resource to create.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdDescription = Lens.field @"description"
{-# INLINEABLE cdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The description of the 'Stage' resource for the 'Deployment' resource to create.
--
-- /Note:/ Consider using 'stageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStageDescription :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdStageDescription = Lens.field @"stageDescription"
{-# INLINEABLE cdStageDescription #-}
{-# DEPRECATED stageDescription "Use generic-lens or generic-optics with 'stageDescription' instead"  #-}

-- | The name of the 'Stage' resource for the 'Deployment' resource to create.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStageName :: Lens.Lens' CreateDeployment (Core.Maybe Core.Text)
cdStageName = Lens.field @"stageName"
{-# INLINEABLE cdStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
--
-- /Note:/ Consider using 'tracingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTracingEnabled :: Lens.Lens' CreateDeployment (Core.Maybe Core.Bool)
cdTracingEnabled = Lens.field @"tracingEnabled"
{-# INLINEABLE cdTracingEnabled #-}
{-# DEPRECATED tracingEnabled "Use generic-lens or generic-optics with 'tracingEnabled' instead"  #-}

-- | A map that defines the stage variables for the 'Stage' resource that is associated with the new deployment. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVariables :: Lens.Lens' CreateDeployment (Core.Maybe (Core.HashMap Core.Text Core.Text))
cdVariables = Lens.field @"variables"
{-# INLINEABLE cdVariables #-}
{-# DEPRECATED variables "Use generic-lens or generic-optics with 'variables' instead"  #-}

instance Core.ToQuery CreateDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDeployment where
        toHeaders CreateDeployment{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateDeployment where
        toJSON CreateDeployment{..}
          = Core.object
              (Core.catMaybes
                 [("cacheClusterEnabled" Core..=) Core.<$> cacheClusterEnabled,
                  ("cacheClusterSize" Core..=) Core.<$> cacheClusterSize,
                  ("canarySettings" Core..=) Core.<$> canarySettings,
                  ("description" Core..=) Core.<$> description,
                  ("stageDescription" Core..=) Core.<$> stageDescription,
                  ("stageName" Core..=) Core.<$> stageName,
                  ("tracingEnabled" Core..=) Core.<$> tracingEnabled,
                  ("variables" Core..=) Core.<$> variables])

instance Core.AWSRequest CreateDeployment where
        type Rs CreateDeployment = Types.Deployment
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/deployments",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
