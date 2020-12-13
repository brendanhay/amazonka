{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Deployment' resource.
module Network.AWS.APIGateway.GetDeployment
  ( -- * Creating a request
    GetDeployment (..),
    mkGetDeployment,

    -- ** Request lenses
    gdfDeploymentId,
    gdfEmbed,
    gdfRestAPIId,

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

-- | Requests API Gateway to get information about a 'Deployment' resource.
--
-- /See:/ 'mkGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { -- | [Required] The identifier of the 'Deployment' resource to get information about.
    deploymentId :: Lude.Text,
    -- | A query parameter to retrieve the specified embedded resources of the returned 'Deployment' resource in the response. In a REST API call, this @embed@ parameter value is a list of comma-separated strings, as in @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=var1,var2@ . The SDK and other platform-dependent libraries might use a different format for the list. Currently, this request supports only retrieval of the embedded API summary this way. Hence, the parameter value must be a single-valued list containing only the @"apisummary"@ string. For example, @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=apisummary@ .
    embed :: Lude.Maybe [Lude.Text],
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeployment' with the minimum fields required to make a request.
--
-- * 'deploymentId' - [Required] The identifier of the 'Deployment' resource to get information about.
-- * 'embed' - A query parameter to retrieve the specified embedded resources of the returned 'Deployment' resource in the response. In a REST API call, this @embed@ parameter value is a list of comma-separated strings, as in @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=var1,var2@ . The SDK and other platform-dependent libraries might use a different format for the list. Currently, this request supports only retrieval of the embedded API summary this way. Hence, the parameter value must be a single-valued list containing only the @"apisummary"@ string. For example, @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=apisummary@ .
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetDeployment ::
  -- | 'deploymentId'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  GetDeployment
mkGetDeployment pDeploymentId_ pRestAPIId_ =
  GetDeployment'
    { deploymentId = pDeploymentId_,
      embed = Lude.Nothing,
      restAPIId = pRestAPIId_
    }

-- | [Required] The identifier of the 'Deployment' resource to get information about.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfDeploymentId :: Lens.Lens' GetDeployment Lude.Text
gdfDeploymentId = Lens.lens (deploymentId :: GetDeployment -> Lude.Text) (\s a -> s {deploymentId = a} :: GetDeployment)
{-# DEPRECATED gdfDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A query parameter to retrieve the specified embedded resources of the returned 'Deployment' resource in the response. In a REST API call, this @embed@ parameter value is a list of comma-separated strings, as in @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=var1,var2@ . The SDK and other platform-dependent libraries might use a different format for the list. Currently, this request supports only retrieval of the embedded API summary this way. Hence, the parameter value must be a single-valued list containing only the @"apisummary"@ string. For example, @GET /restapis/{restapi_id}/deployments/{deployment_id}?embed=apisummary@ .
--
-- /Note:/ Consider using 'embed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfEmbed :: Lens.Lens' GetDeployment (Lude.Maybe [Lude.Text])
gdfEmbed = Lens.lens (embed :: GetDeployment -> Lude.Maybe [Lude.Text]) (\s a -> s {embed = a} :: GetDeployment)
{-# DEPRECATED gdfEmbed "Use generic-lens or generic-optics with 'embed' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfRestAPIId :: Lens.Lens' GetDeployment Lude.Text
gdfRestAPIId = Lens.lens (restAPIId :: GetDeployment -> Lude.Text) (\s a -> s {restAPIId = a} :: GetDeployment)
{-# DEPRECATED gdfRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest GetDeployment where
  type Rs GetDeployment = Deployment
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDeployment where
  toPath GetDeployment' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/deployments/",
        Lude.toBS deploymentId
      ]

instance Lude.ToQuery GetDeployment where
  toQuery GetDeployment' {..} =
    Lude.mconcat
      [ "embed"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> embed)
      ]
