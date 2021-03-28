{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'Stage' resource that references a pre-existing 'Deployment' for the API. 
module Network.AWS.ApiGateway.CreateStage
    (
    -- * Creating a request
      CreateStage (..)
    , mkCreateStage
    -- ** Request lenses
    , cRestApiId
    , cStageName
    , cDeploymentId
    , cCacheClusterEnabled
    , cCacheClusterSize
    , cCanarySettings
    , cDescription
    , cDocumentationVersion
    , cTags
    , cTracingEnabled
    , cVariables

     -- * Destructuring the response
    , Types.Stage (..)
    , Types.mkStage
    -- ** Response lenses
    , Types.sAccessLogSettings
    , Types.sCacheClusterEnabled
    , Types.sCacheClusterSize
    , Types.sCacheClusterStatus
    , Types.sCanarySettings
    , Types.sClientCertificateId
    , Types.sCreatedDate
    , Types.sDeploymentId
    , Types.sDescription
    , Types.sDocumentationVersion
    , Types.sLastUpdatedDate
    , Types.sMethodSettings
    , Types.sStageName
    , Types.sTags
    , Types.sTracingEnabled
    , Types.sVariables
    , Types.sWebAclArn
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to create a 'Stage' resource.
--
-- /See:/ 'mkCreateStage' smart constructor.
data CreateStage = CreateStage'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ [Required] The name for the 'Stage' resource. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
  , deploymentId :: Core.Text
    -- ^ [Required] The identifier of the 'Deployment' resource for the 'Stage' resource.
  , cacheClusterEnabled :: Core.Maybe Core.Bool
    -- ^ Whether cache clustering is enabled for the stage.
  , cacheClusterSize :: Core.Maybe Types.CacheClusterSize
    -- ^ The stage's cache cluster size.
  , canarySettings :: Core.Maybe Types.CanarySettings
    -- ^ The canary deployment settings of this stage.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the 'Stage' resource.
  , documentationVersion :: Core.Maybe Core.Text
    -- ^ The version of the associated API documentation.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  , tracingEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether active tracing with X-ray is enabled for the 'Stage' .
  , variables :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A map that defines the stage variables for the new 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStage' value with any optional fields omitted.
mkCreateStage
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> Core.Text -- ^ 'deploymentId'
    -> CreateStage
mkCreateStage restApiId stageName deploymentId
  = CreateStage'{restApiId, stageName, deploymentId,
                 cacheClusterEnabled = Core.Nothing,
                 cacheClusterSize = Core.Nothing, canarySettings = Core.Nothing,
                 description = Core.Nothing, documentationVersion = Core.Nothing,
                 tags = Core.Nothing, tracingEnabled = Core.Nothing,
                 variables = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRestApiId :: Lens.Lens' CreateStage Core.Text
cRestApiId = Lens.field @"restApiId"
{-# INLINEABLE cRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name for the 'Stage' resource. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStageName :: Lens.Lens' CreateStage Core.Text
cStageName = Lens.field @"stageName"
{-# INLINEABLE cStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | [Required] The identifier of the 'Deployment' resource for the 'Stage' resource.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeploymentId :: Lens.Lens' CreateStage Core.Text
cDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE cDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | Whether cache clustering is enabled for the stage.
--
-- /Note:/ Consider using 'cacheClusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheClusterEnabled :: Lens.Lens' CreateStage (Core.Maybe Core.Bool)
cCacheClusterEnabled = Lens.field @"cacheClusterEnabled"
{-# INLINEABLE cCacheClusterEnabled #-}
{-# DEPRECATED cacheClusterEnabled "Use generic-lens or generic-optics with 'cacheClusterEnabled' instead"  #-}

-- | The stage's cache cluster size.
--
-- /Note:/ Consider using 'cacheClusterSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheClusterSize :: Lens.Lens' CreateStage (Core.Maybe Types.CacheClusterSize)
cCacheClusterSize = Lens.field @"cacheClusterSize"
{-# INLINEABLE cCacheClusterSize #-}
{-# DEPRECATED cacheClusterSize "Use generic-lens or generic-optics with 'cacheClusterSize' instead"  #-}

-- | The canary deployment settings of this stage.
--
-- /Note:/ Consider using 'canarySettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCanarySettings :: Lens.Lens' CreateStage (Core.Maybe Types.CanarySettings)
cCanarySettings = Lens.field @"canarySettings"
{-# INLINEABLE cCanarySettings #-}
{-# DEPRECATED canarySettings "Use generic-lens or generic-optics with 'canarySettings' instead"  #-}

-- | The description of the 'Stage' resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateStage (Core.Maybe Core.Text)
cDescription = Lens.field @"description"
{-# INLINEABLE cDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The version of the associated API documentation.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDocumentationVersion :: Lens.Lens' CreateStage (Core.Maybe Core.Text)
cDocumentationVersion = Lens.field @"documentationVersion"
{-# INLINEABLE cDocumentationVersion #-}
{-# DEPRECATED documentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead"  #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateStage (Core.Maybe (Core.HashMap Core.Text Core.Text))
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
--
-- /Note:/ Consider using 'tracingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTracingEnabled :: Lens.Lens' CreateStage (Core.Maybe Core.Bool)
cTracingEnabled = Lens.field @"tracingEnabled"
{-# INLINEABLE cTracingEnabled #-}
{-# DEPRECATED tracingEnabled "Use generic-lens or generic-optics with 'tracingEnabled' instead"  #-}

-- | A map that defines the stage variables for the new 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVariables :: Lens.Lens' CreateStage (Core.Maybe (Core.HashMap Core.Text Core.Text))
cVariables = Lens.field @"variables"
{-# INLINEABLE cVariables #-}
{-# DEPRECATED variables "Use generic-lens or generic-optics with 'variables' instead"  #-}

instance Core.ToQuery CreateStage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateStage where
        toHeaders CreateStage{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateStage where
        toJSON CreateStage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("stageName" Core..= stageName),
                  Core.Just ("deploymentId" Core..= deploymentId),
                  ("cacheClusterEnabled" Core..=) Core.<$> cacheClusterEnabled,
                  ("cacheClusterSize" Core..=) Core.<$> cacheClusterSize,
                  ("canarySettings" Core..=) Core.<$> canarySettings,
                  ("description" Core..=) Core.<$> description,
                  ("documentationVersion" Core..=) Core.<$> documentationVersion,
                  ("tags" Core..=) Core.<$> tags,
                  ("tracingEnabled" Core..=) Core.<$> tracingEnabled,
                  ("variables" Core..=) Core.<$> variables])

instance Core.AWSRequest CreateStage where
        type Rs CreateStage = Types.Stage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
