{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'Method' resource.
module Network.AWS.APIGateway.DeleteMethod
  ( -- * Creating a request
    DeleteMethod (..),
    mkDeleteMethod,

    -- ** Request lenses
    dmfResourceId,
    dmfHttpMethod,
    dmfRestAPIId,

    -- * Destructuring the response
    DeleteMethodResponse' (..),
    mkDeleteMethodResponse',
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete an existing 'Method' resource.
--
-- /See:/ 'mkDeleteMethod' smart constructor.
data DeleteMethod = DeleteMethod'
  { -- | [Required] The 'Resource' identifier for the 'Method' resource.
    resourceId :: Lude.Text,
    -- | [Required] The HTTP verb of the 'Method' resource.
    httpMethod :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMethod' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
-- * 'httpMethod' - [Required] The HTTP verb of the 'Method' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteMethod ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  DeleteMethod
mkDeleteMethod pResourceId_ pHttpMethod_ pRestAPIId_ =
  DeleteMethod'
    { resourceId = pResourceId_,
      httpMethod = pHttpMethod_,
      restAPIId = pRestAPIId_
    }

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfResourceId :: Lens.Lens' DeleteMethod Lude.Text
dmfResourceId = Lens.lens (resourceId :: DeleteMethod -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteMethod)
{-# DEPRECATED dmfResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfHttpMethod :: Lens.Lens' DeleteMethod Lude.Text
dmfHttpMethod = Lens.lens (httpMethod :: DeleteMethod -> Lude.Text) (\s a -> s {httpMethod = a} :: DeleteMethod)
{-# DEPRECATED dmfHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfRestAPIId :: Lens.Lens' DeleteMethod Lude.Text
dmfRestAPIId = Lens.lens (restAPIId :: DeleteMethod -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteMethod)
{-# DEPRECATED dmfRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest DeleteMethod where
  type Rs DeleteMethod = DeleteMethodResponse'
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteMethodResponse''

instance Lude.ToHeaders DeleteMethod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteMethod where
  toPath DeleteMethod' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod
      ]

instance Lude.ToQuery DeleteMethod where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMethodResponse'' smart constructor.
data DeleteMethodResponse' = DeleteMethodResponse''
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMethodResponse'' with the minimum fields required to make a request.
mkDeleteMethodResponse' ::
  DeleteMethodResponse'
mkDeleteMethodResponse' = DeleteMethodResponse''
