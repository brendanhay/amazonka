{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a resource.
module Network.AWS.APIGateway.GetResource
  ( -- * Creating a request
    GetResource (..),
    mkGetResource,

    -- ** Request lenses
    grEmbed,
    grRestAPIId,
    grResourceId,

    -- * Destructuring the response
    Resource (..),
    mkResource,

    -- ** Response lenses
    rPathPart,
    rPath,
    rId,
    rResourceMethods,
    rParentId,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to list information about a resource.
--
-- /See:/ 'mkGetResource' smart constructor.
data GetResource = GetResource'
  { embed :: Lude.Maybe [Lude.Text],
    restAPIId :: Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResource' with the minimum fields required to make a request.
--
-- * 'embed' - A query parameter to retrieve the specified resources embedded in the returned 'Resource' representation in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources/{resource_id}?embed=methods@ .
-- * 'resourceId' - [Required] The identifier for the 'Resource' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetResource ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  GetResource
mkGetResource pRestAPIId_ pResourceId_ =
  GetResource'
    { embed = Lude.Nothing,
      restAPIId = pRestAPIId_,
      resourceId = pResourceId_
    }

-- | A query parameter to retrieve the specified resources embedded in the returned 'Resource' representation in the response. This @embed@ parameter value is a list of comma-separated strings. Currently, the request supports only retrieval of the embedded 'Method' resources this way. The query parameter value must be a single-valued list and contain the @"methods"@ string. For example, @GET /restapis/{restapi_id}/resources/{resource_id}?embed=methods@ .
--
-- /Note:/ Consider using 'embed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grEmbed :: Lens.Lens' GetResource (Lude.Maybe [Lude.Text])
grEmbed = Lens.lens (embed :: GetResource -> Lude.Maybe [Lude.Text]) (\s a -> s {embed = a} :: GetResource)
{-# DEPRECATED grEmbed "Use generic-lens or generic-optics with 'embed' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRestAPIId :: Lens.Lens' GetResource Lude.Text
grRestAPIId = Lens.lens (restAPIId :: GetResource -> Lude.Text) (\s a -> s {restAPIId = a} :: GetResource)
{-# DEPRECATED grRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The identifier for the 'Resource' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grResourceId :: Lens.Lens' GetResource Lude.Text
grResourceId = Lens.lens (resourceId :: GetResource -> Lude.Text) (\s a -> s {resourceId = a} :: GetResource)
{-# DEPRECATED grResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest GetResource where
  type Rs GetResource = Resource
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetResource where
  toPath GetResource' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId
      ]

instance Lude.ToQuery GetResource where
  toQuery GetResource' {..} =
    Lude.mconcat
      [ "embed"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> embed)
      ]
