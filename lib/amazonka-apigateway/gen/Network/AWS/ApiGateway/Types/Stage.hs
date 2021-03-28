{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.Stage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.Stage
  ( Stage (..)
  -- * Smart constructor
  , mkStage
  -- * Lenses
  , sAccessLogSettings
  , sCacheClusterEnabled
  , sCacheClusterSize
  , sCacheClusterStatus
  , sCanarySettings
  , sClientCertificateId
  , sCreatedDate
  , sDeploymentId
  , sDescription
  , sDocumentationVersion
  , sLastUpdatedDate
  , sMethodSettings
  , sStageName
  , sTags
  , sTracingEnabled
  , sVariables
  , sWebAclArn
  ) where

import qualified Network.AWS.ApiGateway.Types.AccessLogSettings as Types
import qualified Network.AWS.ApiGateway.Types.CacheClusterSize as Types
import qualified Network.AWS.ApiGateway.Types.CacheClusterStatus as Types
import qualified Network.AWS.ApiGateway.Types.CanarySettings as Types
import qualified Network.AWS.ApiGateway.Types.MethodSetting as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a unique identifier for a version of a deployed 'RestApi' that is callable by users.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploy an API> 
--
-- /See:/ 'mkStage' smart constructor.
data Stage = Stage'
  { accessLogSettings :: Core.Maybe Types.AccessLogSettings
    -- ^ Settings for logging access in this stage.
  , cacheClusterEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether a cache cluster is enabled for the stage.
  , cacheClusterSize :: Core.Maybe Types.CacheClusterSize
    -- ^ The size of the cache cluster for the stage, if enabled.
  , cacheClusterStatus :: Core.Maybe Types.CacheClusterStatus
    -- ^ The status of the cache cluster for the stage, if enabled.
  , canarySettings :: Core.Maybe Types.CanarySettings
    -- ^ Settings for the canary deployment in this stage.
  , clientCertificateId :: Core.Maybe Core.Text
    -- ^ The identifier of a client certificate for an API stage.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the stage was created.
  , deploymentId :: Core.Maybe Core.Text
    -- ^ The identifier of the 'Deployment' that the stage points to.
  , description :: Core.Maybe Core.Text
    -- ^ The stage's description.
  , documentationVersion :: Core.Maybe Core.Text
    -- ^ The version of the associated API documentation.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the stage last updated.
  , methodSettings :: Core.Maybe (Core.HashMap Core.Text Types.MethodSetting)
    -- ^ A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage. 
  , stageName :: Core.Maybe Core.Text
    -- ^ The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to API Gateway. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  , tracingEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether active tracing with X-ray is enabled for the 'Stage' .
  , variables :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
  , webAclArn :: Core.Maybe Core.Text
    -- ^ The ARN of the WebAcl associated with the 'Stage' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Stage' value with any optional fields omitted.
mkStage
    :: Stage
mkStage
  = Stage'{accessLogSettings = Core.Nothing,
           cacheClusterEnabled = Core.Nothing,
           cacheClusterSize = Core.Nothing, cacheClusterStatus = Core.Nothing,
           canarySettings = Core.Nothing, clientCertificateId = Core.Nothing,
           createdDate = Core.Nothing, deploymentId = Core.Nothing,
           description = Core.Nothing, documentationVersion = Core.Nothing,
           lastUpdatedDate = Core.Nothing, methodSettings = Core.Nothing,
           stageName = Core.Nothing, tags = Core.Nothing,
           tracingEnabled = Core.Nothing, variables = Core.Nothing,
           webAclArn = Core.Nothing}

-- | Settings for logging access in this stage.
--
-- /Note:/ Consider using 'accessLogSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccessLogSettings :: Lens.Lens' Stage (Core.Maybe Types.AccessLogSettings)
sAccessLogSettings = Lens.field @"accessLogSettings"
{-# INLINEABLE sAccessLogSettings #-}
{-# DEPRECATED accessLogSettings "Use generic-lens or generic-optics with 'accessLogSettings' instead"  #-}

-- | Specifies whether a cache cluster is enabled for the stage.
--
-- /Note:/ Consider using 'cacheClusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterEnabled :: Lens.Lens' Stage (Core.Maybe Core.Bool)
sCacheClusterEnabled = Lens.field @"cacheClusterEnabled"
{-# INLINEABLE sCacheClusterEnabled #-}
{-# DEPRECATED cacheClusterEnabled "Use generic-lens or generic-optics with 'cacheClusterEnabled' instead"  #-}

-- | The size of the cache cluster for the stage, if enabled.
--
-- /Note:/ Consider using 'cacheClusterSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterSize :: Lens.Lens' Stage (Core.Maybe Types.CacheClusterSize)
sCacheClusterSize = Lens.field @"cacheClusterSize"
{-# INLINEABLE sCacheClusterSize #-}
{-# DEPRECATED cacheClusterSize "Use generic-lens or generic-optics with 'cacheClusterSize' instead"  #-}

-- | The status of the cache cluster for the stage, if enabled.
--
-- /Note:/ Consider using 'cacheClusterStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterStatus :: Lens.Lens' Stage (Core.Maybe Types.CacheClusterStatus)
sCacheClusterStatus = Lens.field @"cacheClusterStatus"
{-# INLINEABLE sCacheClusterStatus #-}
{-# DEPRECATED cacheClusterStatus "Use generic-lens or generic-optics with 'cacheClusterStatus' instead"  #-}

-- | Settings for the canary deployment in this stage.
--
-- /Note:/ Consider using 'canarySettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCanarySettings :: Lens.Lens' Stage (Core.Maybe Types.CanarySettings)
sCanarySettings = Lens.field @"canarySettings"
{-# INLINEABLE sCanarySettings #-}
{-# DEPRECATED canarySettings "Use generic-lens or generic-optics with 'canarySettings' instead"  #-}

-- | The identifier of a client certificate for an API stage.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClientCertificateId :: Lens.Lens' Stage (Core.Maybe Core.Text)
sClientCertificateId = Lens.field @"clientCertificateId"
{-# INLINEABLE sClientCertificateId #-}
{-# DEPRECATED clientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead"  #-}

-- | The timestamp when the stage was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedDate :: Lens.Lens' Stage (Core.Maybe Core.NominalDiffTime)
sCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE sCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The identifier of the 'Deployment' that the stage points to.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeploymentId :: Lens.Lens' Stage (Core.Maybe Core.Text)
sDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE sDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The stage's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Stage (Core.Maybe Core.Text)
sDescription = Lens.field @"description"
{-# INLINEABLE sDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The version of the associated API documentation.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocumentationVersion :: Lens.Lens' Stage (Core.Maybe Core.Text)
sDocumentationVersion = Lens.field @"documentationVersion"
{-# INLINEABLE sDocumentationVersion #-}
{-# DEPRECATED documentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead"  #-}

-- | The timestamp when the stage last updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastUpdatedDate :: Lens.Lens' Stage (Core.Maybe Core.NominalDiffTime)
sLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE sLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage. 
--
-- /Note:/ Consider using 'methodSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMethodSettings :: Lens.Lens' Stage (Core.Maybe (Core.HashMap Core.Text Types.MethodSetting))
sMethodSettings = Lens.field @"methodSettings"
{-# INLINEABLE sMethodSettings #-}
{-# DEPRECATED methodSettings "Use generic-lens or generic-optics with 'methodSettings' instead"  #-}

-- | The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to API Gateway. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStageName :: Lens.Lens' Stage (Core.Maybe Core.Text)
sStageName = Lens.field @"stageName"
{-# INLINEABLE sStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Stage (Core.Maybe (Core.HashMap Core.Text Core.Text))
sTags = Lens.field @"tags"
{-# INLINEABLE sTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
--
-- /Note:/ Consider using 'tracingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTracingEnabled :: Lens.Lens' Stage (Core.Maybe Core.Bool)
sTracingEnabled = Lens.field @"tracingEnabled"
{-# INLINEABLE sTracingEnabled #-}
{-# DEPRECATED tracingEnabled "Use generic-lens or generic-optics with 'tracingEnabled' instead"  #-}

-- | A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVariables :: Lens.Lens' Stage (Core.Maybe (Core.HashMap Core.Text Core.Text))
sVariables = Lens.field @"variables"
{-# INLINEABLE sVariables #-}
{-# DEPRECATED variables "Use generic-lens or generic-optics with 'variables' instead"  #-}

-- | The ARN of the WebAcl associated with the 'Stage' .
--
-- /Note:/ Consider using 'webAclArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sWebAclArn :: Lens.Lens' Stage (Core.Maybe Core.Text)
sWebAclArn = Lens.field @"webAclArn"
{-# INLINEABLE sWebAclArn #-}
{-# DEPRECATED webAclArn "Use generic-lens or generic-optics with 'webAclArn' instead"  #-}

instance Core.FromJSON Stage where
        parseJSON
          = Core.withObject "Stage" Core.$
              \ x ->
                Stage' Core.<$>
                  (x Core..:? "accessLogSettings") Core.<*>
                    x Core..:? "cacheClusterEnabled"
                    Core.<*> x Core..:? "cacheClusterSize"
                    Core.<*> x Core..:? "cacheClusterStatus"
                    Core.<*> x Core..:? "canarySettings"
                    Core.<*> x Core..:? "clientCertificateId"
                    Core.<*> x Core..:? "createdDate"
                    Core.<*> x Core..:? "deploymentId"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "documentationVersion"
                    Core.<*> x Core..:? "lastUpdatedDate"
                    Core.<*> x Core..:? "methodSettings"
                    Core.<*> x Core..:? "stageName"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "tracingEnabled"
                    Core.<*> x Core..:? "variables"
                    Core.<*> x Core..:? "webAclArn"
