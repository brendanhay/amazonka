{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Deployment' resource, which makes a specified 'RestApi' callable over the internet.
module Network.AWS.APIGateway.CreateDeployment
  ( -- * Creating a request
    CreateDeployment (..),
    mkCreateDeployment,

    -- ** Request lenses
    cdStageDescription,
    cdVariables,
    cdTracingEnabled,
    cdRestAPIId,
    cdCacheClusterSize,
    cdCanarySettings,
    cdCacheClusterEnabled,
    cdStageName,
    cdDescription,

    -- * Destructuring the response
    Deployment (..),
    mkDeployment,

    -- ** Response lenses
    dApiSummary,
    dCreatedDate,
    dId,
    dDescription,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to create a 'Deployment' resource.
--
-- /See:/ 'mkCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | The description of the 'Stage' resource for the 'Deployment' resource to create.
    stageDescription :: Lude.Maybe Lude.Text,
    -- | A map that defines the stage variables for the 'Stage' resource that is associated with the new deployment. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
    variables :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
    tracingEnabled :: Lude.Maybe Lude.Bool,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | Specifies the cache cluster size for the 'Stage' resource specified in the input, if a cache cluster is enabled.
    cacheClusterSize :: Lude.Maybe CacheClusterSize,
    -- | The input configuration for the canary deployment when the deployment is a canary release deployment.
    canarySettings :: Lude.Maybe DeploymentCanarySettings,
    -- | Enables a cache cluster for the 'Stage' resource specified in the input.
    cacheClusterEnabled :: Lude.Maybe Lude.Bool,
    -- | The name of the 'Stage' resource for the 'Deployment' resource to create.
    stageName :: Lude.Maybe Lude.Text,
    -- | The description for the 'Deployment' resource to create.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- * 'stageDescription' - The description of the 'Stage' resource for the 'Deployment' resource to create.
-- * 'variables' - A map that defines the stage variables for the 'Stage' resource that is associated with the new deployment. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
-- * 'tracingEnabled' - Specifies whether active tracing with X-ray is enabled for the 'Stage' .
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'cacheClusterSize' - Specifies the cache cluster size for the 'Stage' resource specified in the input, if a cache cluster is enabled.
-- * 'canarySettings' - The input configuration for the canary deployment when the deployment is a canary release deployment.
-- * 'cacheClusterEnabled' - Enables a cache cluster for the 'Stage' resource specified in the input.
-- * 'stageName' - The name of the 'Stage' resource for the 'Deployment' resource to create.
-- * 'description' - The description for the 'Deployment' resource to create.
mkCreateDeployment ::
  -- | 'restAPIId'
  Lude.Text ->
  CreateDeployment
mkCreateDeployment pRestAPIId_ =
  CreateDeployment'
    { stageDescription = Lude.Nothing,
      variables = Lude.Nothing,
      tracingEnabled = Lude.Nothing,
      restAPIId = pRestAPIId_,
      cacheClusterSize = Lude.Nothing,
      canarySettings = Lude.Nothing,
      cacheClusterEnabled = Lude.Nothing,
      stageName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The description of the 'Stage' resource for the 'Deployment' resource to create.
--
-- /Note:/ Consider using 'stageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStageDescription :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdStageDescription = Lens.lens (stageDescription :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {stageDescription = a} :: CreateDeployment)
{-# DEPRECATED cdStageDescription "Use generic-lens or generic-optics with 'stageDescription' instead." #-}

-- | A map that defines the stage variables for the 'Stage' resource that is associated with the new deployment. Variable names can have alphanumeric and underscore characters, and the values must match @[A-Za-z0-9-._~:/?#&=,]+@ .
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVariables :: Lens.Lens' CreateDeployment (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdVariables = Lens.lens (variables :: CreateDeployment -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {variables = a} :: CreateDeployment)
{-# DEPRECATED cdVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

-- | Specifies whether active tracing with X-ray is enabled for the 'Stage' .
--
-- /Note:/ Consider using 'tracingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTracingEnabled :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Bool)
cdTracingEnabled = Lens.lens (tracingEnabled :: CreateDeployment -> Lude.Maybe Lude.Bool) (\s a -> s {tracingEnabled = a} :: CreateDeployment)
{-# DEPRECATED cdTracingEnabled "Use generic-lens or generic-optics with 'tracingEnabled' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRestAPIId :: Lens.Lens' CreateDeployment Lude.Text
cdRestAPIId = Lens.lens (restAPIId :: CreateDeployment -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateDeployment)
{-# DEPRECATED cdRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | Specifies the cache cluster size for the 'Stage' resource specified in the input, if a cache cluster is enabled.
--
-- /Note:/ Consider using 'cacheClusterSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCacheClusterSize :: Lens.Lens' CreateDeployment (Lude.Maybe CacheClusterSize)
cdCacheClusterSize = Lens.lens (cacheClusterSize :: CreateDeployment -> Lude.Maybe CacheClusterSize) (\s a -> s {cacheClusterSize = a} :: CreateDeployment)
{-# DEPRECATED cdCacheClusterSize "Use generic-lens or generic-optics with 'cacheClusterSize' instead." #-}

-- | The input configuration for the canary deployment when the deployment is a canary release deployment.
--
-- /Note:/ Consider using 'canarySettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCanarySettings :: Lens.Lens' CreateDeployment (Lude.Maybe DeploymentCanarySettings)
cdCanarySettings = Lens.lens (canarySettings :: CreateDeployment -> Lude.Maybe DeploymentCanarySettings) (\s a -> s {canarySettings = a} :: CreateDeployment)
{-# DEPRECATED cdCanarySettings "Use generic-lens or generic-optics with 'canarySettings' instead." #-}

-- | Enables a cache cluster for the 'Stage' resource specified in the input.
--
-- /Note:/ Consider using 'cacheClusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCacheClusterEnabled :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Bool)
cdCacheClusterEnabled = Lens.lens (cacheClusterEnabled :: CreateDeployment -> Lude.Maybe Lude.Bool) (\s a -> s {cacheClusterEnabled = a} :: CreateDeployment)
{-# DEPRECATED cdCacheClusterEnabled "Use generic-lens or generic-optics with 'cacheClusterEnabled' instead." #-}

-- | The name of the 'Stage' resource for the 'Deployment' resource to create.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStageName :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdStageName = Lens.lens (stageName :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {stageName = a} :: CreateDeployment)
{-# DEPRECATED cdStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The description for the 'Deployment' resource to create.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' CreateDeployment (Lude.Maybe Lude.Text)
cdDescription = Lens.lens (description :: CreateDeployment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateDeployment)
{-# DEPRECATED cdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateDeployment where
  type Rs CreateDeployment = Deployment
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stageDescription" Lude..=) Lude.<$> stageDescription,
            ("variables" Lude..=) Lude.<$> variables,
            ("tracingEnabled" Lude..=) Lude.<$> tracingEnabled,
            ("cacheClusterSize" Lude..=) Lude.<$> cacheClusterSize,
            ("canarySettings" Lude..=) Lude.<$> canarySettings,
            ("cacheClusterEnabled" Lude..=) Lude.<$> cacheClusterEnabled,
            ("stageName" Lude..=) Lude.<$> stageName,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/deployments"]

instance Lude.ToQuery CreateDeployment where
  toQuery = Lude.const Lude.mempty
