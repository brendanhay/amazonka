{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'MethodResponse' resource.
module Network.AWS.APIGateway.DeleteMethodResponse
  ( -- * Creating a request
    DeleteMethodResponse (..),
    mkDeleteMethodResponse,

    -- ** Request lenses
    dmRestAPIId,
    dmResourceId,
    dmHttpMethod,
    dmStatusCode,

    -- * Destructuring the response
    DeleteMethodResponseResponse (..),
    mkDeleteMethodResponseResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to delete an existing 'MethodResponse' resource.
--
-- /See:/ 'mkDeleteMethodResponse' smart constructor.
data DeleteMethodResponse = DeleteMethodResponse'
  { restAPIId ::
      Lude.Text,
    resourceId :: Lude.Text,
    httpMethod :: Lude.Text,
    statusCode :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMethodResponse' with the minimum fields required to make a request.
--
-- * 'httpMethod' - [Required] The HTTP verb of the 'Method' resource.
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'MethodResponse' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'statusCode' - [Required] The status code identifier for the 'MethodResponse' resource.
mkDeleteMethodResponse ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'statusCode'
  Lude.Text ->
  DeleteMethodResponse
mkDeleteMethodResponse
  pRestAPIId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    DeleteMethodResponse'
      { restAPIId = pRestAPIId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmRestAPIId :: Lens.Lens' DeleteMethodResponse Lude.Text
dmRestAPIId = Lens.lens (restAPIId :: DeleteMethodResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteMethodResponse)
{-# DEPRECATED dmRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmResourceId :: Lens.Lens' DeleteMethodResponse Lude.Text
dmResourceId = Lens.lens (resourceId :: DeleteMethodResponse -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteMethodResponse)
{-# DEPRECATED dmResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmHttpMethod :: Lens.Lens' DeleteMethodResponse Lude.Text
dmHttpMethod = Lens.lens (httpMethod :: DeleteMethodResponse -> Lude.Text) (\s a -> s {httpMethod = a} :: DeleteMethodResponse)
{-# DEPRECATED dmHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The status code identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmStatusCode :: Lens.Lens' DeleteMethodResponse Lude.Text
dmStatusCode = Lens.lens (statusCode :: DeleteMethodResponse -> Lude.Text) (\s a -> s {statusCode = a} :: DeleteMethodResponse)
{-# DEPRECATED dmStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.AWSRequest DeleteMethodResponse where
  type Rs DeleteMethodResponse = DeleteMethodResponseResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteMethodResponseResponse'

instance Lude.ToHeaders DeleteMethodResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteMethodResponse where
  toPath DeleteMethodResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/responses/",
        Lude.toBS statusCode
      ]

instance Lude.ToQuery DeleteMethodResponse where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMethodResponseResponse' smart constructor.
data DeleteMethodResponseResponse = DeleteMethodResponseResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMethodResponseResponse' with the minimum fields required to make a request.
mkDeleteMethodResponseResponse ::
  DeleteMethodResponseResponse
mkDeleteMethodResponseResponse = DeleteMethodResponseResponse'
