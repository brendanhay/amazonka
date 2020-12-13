{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration.
module Network.AWS.APIGateway.DeleteIntegration
  ( -- * Creating a request
    DeleteIntegration (..),
    mkDeleteIntegration,

    -- ** Request lenses
    difResourceId,
    difHttpMethod,
    difRestAPIId,

    -- * Destructuring the response
    DeleteIntegrationResponse' (..),
    mkDeleteIntegrationResponse',
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a delete integration request.
--
-- /See:/ 'mkDeleteIntegration' smart constructor.
data DeleteIntegration = DeleteIntegration'
  { -- | [Required] Specifies a delete integration request's resource identifier.
    resourceId :: Lude.Text,
    -- | [Required] Specifies a delete integration request's HTTP method.
    httpMethod :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntegration' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] Specifies a delete integration request's resource identifier.
-- * 'httpMethod' - [Required] Specifies a delete integration request's HTTP method.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteIntegration ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  DeleteIntegration
mkDeleteIntegration pResourceId_ pHttpMethod_ pRestAPIId_ =
  DeleteIntegration'
    { resourceId = pResourceId_,
      httpMethod = pHttpMethod_,
      restAPIId = pRestAPIId_
    }

-- | [Required] Specifies a delete integration request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difResourceId :: Lens.Lens' DeleteIntegration Lude.Text
difResourceId = Lens.lens (resourceId :: DeleteIntegration -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteIntegration)
{-# DEPRECATED difResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies a delete integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difHttpMethod :: Lens.Lens' DeleteIntegration Lude.Text
difHttpMethod = Lens.lens (httpMethod :: DeleteIntegration -> Lude.Text) (\s a -> s {httpMethod = a} :: DeleteIntegration)
{-# DEPRECATED difHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difRestAPIId :: Lens.Lens' DeleteIntegration Lude.Text
difRestAPIId = Lens.lens (restAPIId :: DeleteIntegration -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteIntegration)
{-# DEPRECATED difRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest DeleteIntegration where
  type Rs DeleteIntegration = DeleteIntegrationResponse'
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteIntegrationResponse''

instance Lude.ToHeaders DeleteIntegration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteIntegration where
  toPath DeleteIntegration' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/integration"
      ]

instance Lude.ToQuery DeleteIntegration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIntegrationResponse'' smart constructor.
data DeleteIntegrationResponse' = DeleteIntegrationResponse''
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntegrationResponse'' with the minimum fields required to make a request.
mkDeleteIntegrationResponse' ::
  DeleteIntegrationResponse'
mkDeleteIntegrationResponse' = DeleteIntegrationResponse''
