{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Resource' resource.
module Network.AWS.APIGateway.CreateResource
  ( -- * Creating a request
    CreateResource (..),
    mkCreateResource,

    -- ** Request lenses
    crRestAPIId,
    crParentId,
    crPathPart,

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

-- | Requests API Gateway to create a 'Resource' resource.
--
-- /See:/ 'mkCreateResource' smart constructor.
data CreateResource = CreateResource'
  { restAPIId :: Lude.Text,
    parentId :: Lude.Text,
    pathPart :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResource' with the minimum fields required to make a request.
--
-- * 'parentId' - [Required] The parent resource's identifier.
-- * 'pathPart' - The last path segment for this resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkCreateResource ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'parentId'
  Lude.Text ->
  -- | 'pathPart'
  Lude.Text ->
  CreateResource
mkCreateResource pRestAPIId_ pParentId_ pPathPart_ =
  CreateResource'
    { restAPIId = pRestAPIId_,
      parentId = pParentId_,
      pathPart = pPathPart_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRestAPIId :: Lens.Lens' CreateResource Lude.Text
crRestAPIId = Lens.lens (restAPIId :: CreateResource -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateResource)
{-# DEPRECATED crRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The parent resource's identifier.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crParentId :: Lens.Lens' CreateResource Lude.Text
crParentId = Lens.lens (parentId :: CreateResource -> Lude.Text) (\s a -> s {parentId = a} :: CreateResource)
{-# DEPRECATED crParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The last path segment for this resource.
--
-- /Note:/ Consider using 'pathPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPathPart :: Lens.Lens' CreateResource Lude.Text
crPathPart = Lens.lens (pathPart :: CreateResource -> Lude.Text) (\s a -> s {pathPart = a} :: CreateResource)
{-# DEPRECATED crPathPart "Use generic-lens or generic-optics with 'pathPart' instead." #-}

instance Lude.AWSRequest CreateResource where
  type Rs CreateResource = Resource
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateResource where
  toJSON CreateResource' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("pathPart" Lude..= pathPart)])

instance Lude.ToPath CreateResource where
  toPath CreateResource' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS parentId
      ]

instance Lude.ToQuery CreateResource where
  toQuery = Lude.const Lude.mempty
