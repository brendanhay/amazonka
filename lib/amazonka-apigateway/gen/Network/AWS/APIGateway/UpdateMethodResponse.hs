{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'MethodResponse' resource.
module Network.AWS.APIGateway.UpdateMethodResponse
  ( -- * Creating a request
    UpdateMethodResponse (..),
    mkUpdateMethodResponse,

    -- ** Request lenses
    umPatchOperations,
    umRestAPIId,
    umResourceId,
    umHttpMethod,
    umStatusCode,

    -- * Destructuring the response
    MethodResponse (..),
    mkMethodResponse,

    -- ** Response lenses
    mResponseModels,
    mStatusCode,
    mResponseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to update an existing 'MethodResponse' resource.
--
-- /See:/ 'mkUpdateMethodResponse' smart constructor.
data UpdateMethodResponse = UpdateMethodResponse'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text,
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

-- | Creates a value of 'UpdateMethodResponse' with the minimum fields required to make a request.
--
-- * 'httpMethod' - [Required] The HTTP verb of the 'Method' resource.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'MethodResponse' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'statusCode' - [Required] The status code for the 'MethodResponse' resource.
mkUpdateMethodResponse ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'statusCode'
  Lude.Text ->
  UpdateMethodResponse
mkUpdateMethodResponse
  pRestAPIId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    UpdateMethodResponse'
      { patchOperations = Lude.Nothing,
        restAPIId = pRestAPIId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umPatchOperations :: Lens.Lens' UpdateMethodResponse (Lude.Maybe [PatchOperation])
umPatchOperations = Lens.lens (patchOperations :: UpdateMethodResponse -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateMethodResponse)
{-# DEPRECATED umPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umRestAPIId :: Lens.Lens' UpdateMethodResponse Lude.Text
umRestAPIId = Lens.lens (restAPIId :: UpdateMethodResponse -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateMethodResponse)
{-# DEPRECATED umRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umResourceId :: Lens.Lens' UpdateMethodResponse Lude.Text
umResourceId = Lens.lens (resourceId :: UpdateMethodResponse -> Lude.Text) (\s a -> s {resourceId = a} :: UpdateMethodResponse)
{-# DEPRECATED umResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umHttpMethod :: Lens.Lens' UpdateMethodResponse Lude.Text
umHttpMethod = Lens.lens (httpMethod :: UpdateMethodResponse -> Lude.Text) (\s a -> s {httpMethod = a} :: UpdateMethodResponse)
{-# DEPRECATED umHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The status code for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umStatusCode :: Lens.Lens' UpdateMethodResponse Lude.Text
umStatusCode = Lens.lens (statusCode :: UpdateMethodResponse -> Lude.Text) (\s a -> s {statusCode = a} :: UpdateMethodResponse)
{-# DEPRECATED umStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.AWSRequest UpdateMethodResponse where
  type Rs UpdateMethodResponse = MethodResponse
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateMethodResponse where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateMethodResponse where
  toJSON UpdateMethodResponse' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateMethodResponse where
  toPath UpdateMethodResponse' {..} =
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

instance Lude.ToQuery UpdateMethodResponse where
  toQuery = Lude.const Lude.mempty
