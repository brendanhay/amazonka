{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a 'RequestValidator' of a given 'RestApi' .
module Network.AWS.APIGateway.UpdateRequestValidator
  ( -- * Creating a request
    UpdateRequestValidator (..),
    mkUpdateRequestValidator,

    -- ** Request lenses
    urvPatchOperations,
    urvRestAPIId,
    urvRequestValidatorId,

    -- * Destructuring the response
    RequestValidator (..),
    mkRequestValidator,

    -- ** Response lenses
    rvValidateRequestParameters,
    rvName,
    rvValidateRequestBody,
    rvId,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Updates a 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkUpdateRequestValidator' smart constructor.
data UpdateRequestValidator = UpdateRequestValidator'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    restAPIId :: Lude.Text,
    requestValidatorId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRequestValidator' with the minimum fields required to make a request.
--
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
-- * 'requestValidatorId' - [Required] The identifier of 'RequestValidator' to be updated.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkUpdateRequestValidator ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'requestValidatorId'
  Lude.Text ->
  UpdateRequestValidator
mkUpdateRequestValidator pRestAPIId_ pRequestValidatorId_ =
  UpdateRequestValidator'
    { patchOperations = Lude.Nothing,
      restAPIId = pRestAPIId_,
      requestValidatorId = pRequestValidatorId_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urvPatchOperations :: Lens.Lens' UpdateRequestValidator (Lude.Maybe [PatchOperation])
urvPatchOperations = Lens.lens (patchOperations :: UpdateRequestValidator -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateRequestValidator)
{-# DEPRECATED urvPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urvRestAPIId :: Lens.Lens' UpdateRequestValidator Lude.Text
urvRestAPIId = Lens.lens (restAPIId :: UpdateRequestValidator -> Lude.Text) (\s a -> s {restAPIId = a} :: UpdateRequestValidator)
{-# DEPRECATED urvRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The identifier of 'RequestValidator' to be updated.
--
-- /Note:/ Consider using 'requestValidatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urvRequestValidatorId :: Lens.Lens' UpdateRequestValidator Lude.Text
urvRequestValidatorId = Lens.lens (requestValidatorId :: UpdateRequestValidator -> Lude.Text) (\s a -> s {requestValidatorId = a} :: UpdateRequestValidator)
{-# DEPRECATED urvRequestValidatorId "Use generic-lens or generic-optics with 'requestValidatorId' instead." #-}

instance Lude.AWSRequest UpdateRequestValidator where
  type Rs UpdateRequestValidator = RequestValidator
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateRequestValidator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateRequestValidator where
  toJSON UpdateRequestValidator' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateRequestValidator where
  toPath UpdateRequestValidator' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/requestvalidators/",
        Lude.toBS requestValidatorId
      ]

instance Lude.ToQuery UpdateRequestValidator where
  toQuery = Lude.const Lude.mempty
