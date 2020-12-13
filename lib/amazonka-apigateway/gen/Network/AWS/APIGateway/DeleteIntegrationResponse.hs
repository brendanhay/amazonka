{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteIntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration response.
module Network.AWS.APIGateway.DeleteIntegrationResponse
  ( -- * Creating a request
    DeleteIntegrationResponse (..),
    mkDeleteIntegrationResponse,

    -- ** Request lenses
    diResourceId,
    diHttpMethod,
    diRestAPIId,
    diStatusCode,

    -- * Destructuring the response
    DeleteIntegrationResponseResponse (..),
    mkDeleteIntegrationResponseResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a delete integration response request.
--
-- /See:/ 'mkDeleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { -- | [Required] Specifies a delete integration response request's resource identifier.
    resourceId :: Lude.Text,
    -- | [Required] Specifies a delete integration response request's HTTP method.
    httpMethod :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] Specifies a delete integration response request's status code.
    statusCode :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntegrationResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] Specifies a delete integration response request's resource identifier.
-- * 'httpMethod' - [Required] Specifies a delete integration response request's HTTP method.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'statusCode' - [Required] Specifies a delete integration response request's status code.
mkDeleteIntegrationResponse ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'statusCode'
  Lude.Text ->
  DeleteIntegrationResponse
mkDeleteIntegrationResponse
  pResourceId_
  pHttpMethod_
  pRestAPIId_
  pStatusCode_ =
    DeleteIntegrationResponse'
      { resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        restAPIId = pRestAPIId_,
        statusCode = pStatusCode_
      }

-- | [Required] Specifies a delete integration response request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diResourceId :: Lens.Lens' DeleteIntegrationResponse Lude.Text
diResourceId = Lens.lens (resourceId :: DeleteIntegrationResponse -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteIntegrationResponse)
{-# DEPRECATED diResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies a delete integration response request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diHttpMethod :: Lens.Lens' DeleteIntegrationResponse Lude.Text
diHttpMethod = Lens.lens (httpMethod :: DeleteIntegrationResponse -> Lude.Text) (\s a -> s {httpMethod = a} :: DeleteIntegrationResponse)
{-# DEPRECATED diHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diRestAPIId :: Lens.Lens' DeleteIntegrationResponse Lude.Text
diRestAPIId = Lens.lens (restAPIId :: DeleteIntegrationResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteIntegrationResponse)
{-# DEPRECATED diRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Specifies a delete integration response request's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStatusCode :: Lens.Lens' DeleteIntegrationResponse Lude.Text
diStatusCode = Lens.lens (statusCode :: DeleteIntegrationResponse -> Lude.Text) (\s a -> s {statusCode = a} :: DeleteIntegrationResponse)
{-# DEPRECATED diStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.AWSRequest DeleteIntegrationResponse where
  type
    Rs DeleteIntegrationResponse =
      DeleteIntegrationResponseResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteIntegrationResponseResponse'

instance Lude.ToHeaders DeleteIntegrationResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteIntegrationResponse where
  toPath DeleteIntegrationResponse' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/integration/responses/",
        Lude.toBS statusCode
      ]

instance Lude.ToQuery DeleteIntegrationResponse where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntegrationResponseResponse' with the minimum fields required to make a request.
mkDeleteIntegrationResponseResponse ::
  DeleteIntegrationResponseResponse
mkDeleteIntegrationResponseResponse =
  DeleteIntegrationResponseResponse'
