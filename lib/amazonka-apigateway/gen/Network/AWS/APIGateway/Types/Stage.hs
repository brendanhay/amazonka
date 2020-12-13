{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Stage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Stage
  ( Stage (..),

    -- * Smart constructor
    mkStage,

    -- * Lenses
    sDeploymentId,
    sVariables,
    sAccessLogSettings,
    sDocumentationVersion,
    sClientCertificateId,
    sTracingEnabled,
    sCreatedDate,
    sCacheClusterStatus,
    sMethodSettings,
    sLastUpdatedDate,
    sCacheClusterSize,
    sWebACLARN,
    sCanarySettings,
    sCacheClusterEnabled,
    sStageName,
    sDescription,
    sTags,
  )
where

import Network.AWS.APIGateway.Types.AccessLogSettings
import Network.AWS.APIGateway.Types.CacheClusterSize
import Network.AWS.APIGateway.Types.CacheClusterStatus
import Network.AWS.APIGateway.Types.CanarySettings
import Network.AWS.APIGateway.Types.MethodSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a unique identifier for a version of a deployed 'RestApi' that is callable by users.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploy an API>
--
-- /See:/ 'mkStage' smart constructor.
data Stage = Stage'
  { -- | The identifier of the 'Deployment' that the stage points to.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
    variables :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Settings for logging access in this stage.
    accessLogSettings :: Lude.Maybe AccessLogSettings,
    -- | The version of the associated API documentation.
    documentationVersion :: Lude.Maybe Lude.Text,
    -- | The identifier of a client certificate for an API stage.
    clientCertificateId :: Lude.Maybe Lude.Text,
    -- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
    tracingEnabled :: Lude.Maybe Lude.Bool,
    -- | The timestamp when the stage was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The status of the cache cluster for the stage, if enabled.
    cacheClusterStatus :: Lude.Maybe CacheClusterStatus,
    -- | A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage.
    methodSettings :: Lude.Maybe (Lude.HashMap Lude.Text (MethodSetting)),
    -- | The timestamp when the stage last updated.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | The size of the cache cluster for the stage, if enabled.
    cacheClusterSize :: Lude.Maybe CacheClusterSize,
    -- | The ARN of the WebAcl associated with the 'Stage' .
    webACLARN :: Lude.Maybe Lude.Text,
    -- | Settings for the canary deployment in this stage.
    canarySettings :: Lude.Maybe CanarySettings,
    -- | Specifies whether a cache cluster is enabled for the stage.
    cacheClusterEnabled :: Lude.Maybe Lude.Bool,
    -- | The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to API Gateway. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
    stageName :: Lude.Maybe Lude.Text,
    -- | The stage's description.
    description :: Lude.Maybe Lude.Text,
    -- | The collection of tags. Each tag element is associated with a given resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stage' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The identifier of the 'Deployment' that the stage points to.
-- * 'variables' - A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
-- * 'accessLogSettings' - Settings for logging access in this stage.
-- * 'documentationVersion' - The version of the associated API documentation.
-- * 'clientCertificateId' - The identifier of a client certificate for an API stage.
-- * 'tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the 'Stage' .
-- * 'createdDate' - The timestamp when the stage was created.
-- * 'cacheClusterStatus' - The status of the cache cluster for the stage, if enabled.
-- * 'methodSettings' - A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage.
-- * 'lastUpdatedDate' - The timestamp when the stage last updated.
-- * 'cacheClusterSize' - The size of the cache cluster for the stage, if enabled.
-- * 'webACLARN' - The ARN of the WebAcl associated with the 'Stage' .
-- * 'canarySettings' - Settings for the canary deployment in this stage.
-- * 'cacheClusterEnabled' - Specifies whether a cache cluster is enabled for the stage.
-- * 'stageName' - The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to API Gateway. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
-- * 'description' - The stage's description.
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
mkStage ::
  Stage
mkStage =
  Stage'
    { deploymentId = Lude.Nothing,
      variables = Lude.Nothing,
      accessLogSettings = Lude.Nothing,
      documentationVersion = Lude.Nothing,
      clientCertificateId = Lude.Nothing,
      tracingEnabled = Lude.Nothing,
      createdDate = Lude.Nothing,
      cacheClusterStatus = Lude.Nothing,
      methodSettings = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      cacheClusterSize = Lude.Nothing,
      webACLARN = Lude.Nothing,
      canarySettings = Lude.Nothing,
      cacheClusterEnabled = Lude.Nothing,
      stageName = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The identifier of the 'Deployment' that the stage points to.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeploymentId :: Lens.Lens' Stage (Lude.Maybe Lude.Text)
sDeploymentId = Lens.lens (deploymentId :: Stage -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: Stage)
{-# DEPRECATED sDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A map that defines the stage variables for a 'Stage' resource. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVariables :: Lens.Lens' Stage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sVariables = Lens.lens (variables :: Stage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {variables = a} :: Stage)
{-# DEPRECATED sVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

-- | Settings for logging access in this stage.
--
-- /Note:/ Consider using 'accessLogSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccessLogSettings :: Lens.Lens' Stage (Lude.Maybe AccessLogSettings)
sAccessLogSettings = Lens.lens (accessLogSettings :: Stage -> Lude.Maybe AccessLogSettings) (\s a -> s {accessLogSettings = a} :: Stage)
{-# DEPRECATED sAccessLogSettings "Use generic-lens or generic-optics with 'accessLogSettings' instead." #-}

-- | The version of the associated API documentation.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocumentationVersion :: Lens.Lens' Stage (Lude.Maybe Lude.Text)
sDocumentationVersion = Lens.lens (documentationVersion :: Stage -> Lude.Maybe Lude.Text) (\s a -> s {documentationVersion = a} :: Stage)
{-# DEPRECATED sDocumentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead." #-}

-- | The identifier of a client certificate for an API stage.
--
-- /Note:/ Consider using 'clientCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClientCertificateId :: Lens.Lens' Stage (Lude.Maybe Lude.Text)
sClientCertificateId = Lens.lens (clientCertificateId :: Stage -> Lude.Maybe Lude.Text) (\s a -> s {clientCertificateId = a} :: Stage)
{-# DEPRECATED sClientCertificateId "Use generic-lens or generic-optics with 'clientCertificateId' instead." #-}

-- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
--
-- /Note:/ Consider using 'tracingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTracingEnabled :: Lens.Lens' Stage (Lude.Maybe Lude.Bool)
sTracingEnabled = Lens.lens (tracingEnabled :: Stage -> Lude.Maybe Lude.Bool) (\s a -> s {tracingEnabled = a} :: Stage)
{-# DEPRECATED sTracingEnabled "Use generic-lens or generic-optics with 'tracingEnabled' instead." #-}

-- | The timestamp when the stage was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedDate :: Lens.Lens' Stage (Lude.Maybe Lude.Timestamp)
sCreatedDate = Lens.lens (createdDate :: Stage -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: Stage)
{-# DEPRECATED sCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The status of the cache cluster for the stage, if enabled.
--
-- /Note:/ Consider using 'cacheClusterStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterStatus :: Lens.Lens' Stage (Lude.Maybe CacheClusterStatus)
sCacheClusterStatus = Lens.lens (cacheClusterStatus :: Stage -> Lude.Maybe CacheClusterStatus) (\s a -> s {cacheClusterStatus = a} :: Stage)
{-# DEPRECATED sCacheClusterStatus "Use generic-lens or generic-optics with 'cacheClusterStatus' instead." #-}

-- | A map that defines the method settings for a 'Stage' resource. Keys (designated as @/{method_setting_key@ below) are method paths defined as @{resource_path}/{http_method}@ for an individual method override, or @/\*/\*@ for overriding all methods in the stage.
--
-- /Note:/ Consider using 'methodSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMethodSettings :: Lens.Lens' Stage (Lude.Maybe (Lude.HashMap Lude.Text (MethodSetting)))
sMethodSettings = Lens.lens (methodSettings :: Stage -> Lude.Maybe (Lude.HashMap Lude.Text (MethodSetting))) (\s a -> s {methodSettings = a} :: Stage)
{-# DEPRECATED sMethodSettings "Use generic-lens or generic-optics with 'methodSettings' instead." #-}

-- | The timestamp when the stage last updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastUpdatedDate :: Lens.Lens' Stage (Lude.Maybe Lude.Timestamp)
sLastUpdatedDate = Lens.lens (lastUpdatedDate :: Stage -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: Stage)
{-# DEPRECATED sLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The size of the cache cluster for the stage, if enabled.
--
-- /Note:/ Consider using 'cacheClusterSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterSize :: Lens.Lens' Stage (Lude.Maybe CacheClusterSize)
sCacheClusterSize = Lens.lens (cacheClusterSize :: Stage -> Lude.Maybe CacheClusterSize) (\s a -> s {cacheClusterSize = a} :: Stage)
{-# DEPRECATED sCacheClusterSize "Use generic-lens or generic-optics with 'cacheClusterSize' instead." #-}

-- | The ARN of the WebAcl associated with the 'Stage' .
--
-- /Note:/ Consider using 'webACLARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sWebACLARN :: Lens.Lens' Stage (Lude.Maybe Lude.Text)
sWebACLARN = Lens.lens (webACLARN :: Stage -> Lude.Maybe Lude.Text) (\s a -> s {webACLARN = a} :: Stage)
{-# DEPRECATED sWebACLARN "Use generic-lens or generic-optics with 'webACLARN' instead." #-}

-- | Settings for the canary deployment in this stage.
--
-- /Note:/ Consider using 'canarySettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCanarySettings :: Lens.Lens' Stage (Lude.Maybe CanarySettings)
sCanarySettings = Lens.lens (canarySettings :: Stage -> Lude.Maybe CanarySettings) (\s a -> s {canarySettings = a} :: Stage)
{-# DEPRECATED sCanarySettings "Use generic-lens or generic-optics with 'canarySettings' instead." #-}

-- | Specifies whether a cache cluster is enabled for the stage.
--
-- /Note:/ Consider using 'cacheClusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCacheClusterEnabled :: Lens.Lens' Stage (Lude.Maybe Lude.Bool)
sCacheClusterEnabled = Lens.lens (cacheClusterEnabled :: Stage -> Lude.Maybe Lude.Bool) (\s a -> s {cacheClusterEnabled = a} :: Stage)
{-# DEPRECATED sCacheClusterEnabled "Use generic-lens or generic-optics with 'cacheClusterEnabled' instead." #-}

-- | The name of the stage is the first path segment in the Uniform Resource Identifier (URI) of a call to API Gateway. Stage names can only contain alphanumeric characters, hyphens, and underscores. Maximum length is 128 characters.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStageName :: Lens.Lens' Stage (Lude.Maybe Lude.Text)
sStageName = Lens.lens (stageName :: Stage -> Lude.Maybe Lude.Text) (\s a -> s {stageName = a} :: Stage)
{-# DEPRECATED sStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The stage's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Stage (Lude.Maybe Lude.Text)
sDescription = Lens.lens (description :: Stage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Stage)
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Stage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sTags = Lens.lens (tags :: Stage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Stage)
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Stage where
  parseJSON =
    Lude.withObject
      "Stage"
      ( \x ->
          Stage'
            Lude.<$> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "variables" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "accessLogSettings")
            Lude.<*> (x Lude..:? "documentationVersion")
            Lude.<*> (x Lude..:? "clientCertificateId")
            Lude.<*> (x Lude..:? "tracingEnabled")
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "cacheClusterStatus")
            Lude.<*> (x Lude..:? "methodSettings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lastUpdatedDate")
            Lude.<*> (x Lude..:? "cacheClusterSize")
            Lude.<*> (x Lude..:? "webAclArn")
            Lude.<*> (x Lude..:? "canarySettings")
            Lude.<*> (x Lude..:? "cacheClusterEnabled")
            Lude.<*> (x Lude..:? "stageName")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
