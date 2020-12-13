{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'Method' resource.
module Network.AWS.APIGateway.UpdateMethod
  ( -- * Creating a request
    UpdateMethod (..),
    mkUpdateMethod,

    -- ** Request lenses
    umfResourceId,
    umfHttpMethod,
    umfRestAPIId,
    umfPatchOperations,

    -- * Destructuring the response
    Method (..),
    mkMethod,

    -- ** Response lenses
    mMethodResponses,
    mHttpMethod,
    mAuthorizationScopes,
    mRequestValidatorId,
    mRequestModels,
    mRequestParameters,
    mAuthorizerId,
    mOperationName,
    mAuthorizationType,
    mApiKeyRequired,
    mMethodIntegration,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to update an existing 'Method' resource.
--
-- /See:/ 'mkUpdateMethod' smart constructor.
data UpdateMethod = UpdateMethod'
  { -- | [Required] The 'Resource' identifier for the 'Method' resource.
    resourceId :: Lude.Text,
    -- | [Required] The HTTP verb of the 'Method' resource.
    httpMethod :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | A list of update operations to be applied to the specified resource and in the order specified in this list.
    patchOperations :: Lude.Maybe [PatchOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMethod' with the minimum fields required to make a request.
--
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
-- * 'httpMethod' - [Required] The HTTP verb of the 'Method' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateMethod ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  UpdateMethod
mkUpdateMethod pResourceId_ pHttpMethod_ pRestAPIId_ =
  UpdateMethod'
    { resourceId = pResourceId_,
      httpMethod = pHttpMethod_,
      restAPIId = pRestAPIId_,
      patchOperations = Lude.Nothing
    }

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfResourceId :: Lens.Lens' UpdateMethod Lude.Text
umfResourceId = Lens.lens (resourceId :: UpdateMethod -> Lude.Text) (\s a -> s {resourceId = a} :: UpdateMethod)
{-# DEPRECATED umfResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfHttpMethod :: Lens.Lens' UpdateMethod Lude.Text
umfHttpMethod = Lens.lens (httpMethod :: UpdateMethod -> Lude.Text) (\s a -> s {httpMethod = a} :: UpdateMethod)
{-# DEPRECATED umfHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfRestAPIId :: Lens.Lens' UpdateMethod Lude.Text
umfRestAPIId = Lens.lens (restAPIId :: UpdateMethod -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateMethod)
{-# DEPRECATED umfRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfPatchOperations :: Lens.Lens' UpdateMethod (Lude.Maybe [PatchOperation])
umfPatchOperations = Lens.lens (patchOperations :: UpdateMethod -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateMethod)
{-# DEPRECATED umfPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

instance Lude.AWSRequest UpdateMethod where
  type Rs UpdateMethod = Method
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateMethod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateMethod where
  toJSON UpdateMethod' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateMethod where
  toPath UpdateMethod' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod
      ]

instance Lude.ToQuery UpdateMethod where
  toQuery = Lude.const Lude.mempty
