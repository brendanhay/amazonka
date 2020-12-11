{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ummPatchOperations,
    ummRestAPIId,
    ummResourceId,
    ummHttpMethod,

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
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text,
    resourceId :: Lude.Text,
    httpMethod :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMethod' with the minimum fields required to make a request.
--
-- * 'httpMethod' - [Required] The HTTP verb of the 'Method' resource.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkUpdateMethod ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  UpdateMethod
mkUpdateMethod pRestAPIId_ pResourceId_ pHttpMethod_ =
  UpdateMethod'
    { patchOperations = Lude.Nothing,
      restAPIId = pRestAPIId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ummPatchOperations :: Lens.Lens' UpdateMethod (Lude.Maybe [PatchOperation])
ummPatchOperations = Lens.lens (patchOperations :: UpdateMethod -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateMethod)
{-# DEPRECATED ummPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ummRestAPIId :: Lens.Lens' UpdateMethod Lude.Text
ummRestAPIId = Lens.lens (restAPIId :: UpdateMethod -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateMethod)
{-# DEPRECATED ummRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ummResourceId :: Lens.Lens' UpdateMethod Lude.Text
ummResourceId = Lens.lens (resourceId :: UpdateMethod -> Lude.Text) (\s a -> s {resourceId = a} :: UpdateMethod)
{-# DEPRECATED ummResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ummHttpMethod :: Lens.Lens' UpdateMethod Lude.Text
ummHttpMethod = Lens.lens (httpMethod :: UpdateMethod -> Lude.Text) (\s a -> s {httpMethod = a} :: UpdateMethod)
{-# DEPRECATED ummHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

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
