{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'ReqeustValidator' of a given 'RestApi' .
module Network.AWS.APIGateway.CreateRequestValidator
  ( -- * Creating a request
    CreateRequestValidator (..),
    mkCreateRequestValidator,

    -- ** Request lenses
    crvValidateRequestParameters,
    crvName,
    crvValidateRequestBody,
    crvRestAPIId,

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

-- | Creates a 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkCreateRequestValidator' smart constructor.
data CreateRequestValidator = CreateRequestValidator'
  { validateRequestParameters ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    validateRequestBody :: Lude.Maybe Lude.Bool,
    restAPIId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRequestValidator' with the minimum fields required to make a request.
--
-- * 'name' - The name of the to-be-created 'RequestValidator' .
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'validateRequestBody' - A Boolean flag to indicate whether to validate request body according to the configured model schema for the method (@true@ ) or not (@false@ ).
-- * 'validateRequestParameters' - A Boolean flag to indicate whether to validate request parameters, @true@ , or not @false@ .
mkCreateRequestValidator ::
  -- | 'restAPIId'
  Lude.Text ->
  CreateRequestValidator
mkCreateRequestValidator pRestAPIId_ =
  CreateRequestValidator'
    { validateRequestParameters = Lude.Nothing,
      name = Lude.Nothing,
      validateRequestBody = Lude.Nothing,
      restAPIId = pRestAPIId_
    }

-- | A Boolean flag to indicate whether to validate request parameters, @true@ , or not @false@ .
--
-- /Note:/ Consider using 'validateRequestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvValidateRequestParameters :: Lens.Lens' CreateRequestValidator (Lude.Maybe Lude.Bool)
crvValidateRequestParameters = Lens.lens (validateRequestParameters :: CreateRequestValidator -> Lude.Maybe Lude.Bool) (\s a -> s {validateRequestParameters = a} :: CreateRequestValidator)
{-# DEPRECATED crvValidateRequestParameters "Use generic-lens or generic-optics with 'validateRequestParameters' instead." #-}

-- | The name of the to-be-created 'RequestValidator' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvName :: Lens.Lens' CreateRequestValidator (Lude.Maybe Lude.Text)
crvName = Lens.lens (name :: CreateRequestValidator -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateRequestValidator)
{-# DEPRECATED crvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A Boolean flag to indicate whether to validate request body according to the configured model schema for the method (@true@ ) or not (@false@ ).
--
-- /Note:/ Consider using 'validateRequestBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvValidateRequestBody :: Lens.Lens' CreateRequestValidator (Lude.Maybe Lude.Bool)
crvValidateRequestBody = Lens.lens (validateRequestBody :: CreateRequestValidator -> Lude.Maybe Lude.Bool) (\s a -> s {validateRequestBody = a} :: CreateRequestValidator)
{-# DEPRECATED crvValidateRequestBody "Use generic-lens or generic-optics with 'validateRequestBody' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crvRestAPIId :: Lens.Lens' CreateRequestValidator Lude.Text
crvRestAPIId = Lens.lens (restAPIId :: CreateRequestValidator -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateRequestValidator)
{-# DEPRECATED crvRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest CreateRequestValidator where
  type Rs CreateRequestValidator = RequestValidator
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateRequestValidator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateRequestValidator where
  toJSON CreateRequestValidator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("validateRequestParameters" Lude..=)
              Lude.<$> validateRequestParameters,
            ("name" Lude..=) Lude.<$> name,
            ("validateRequestBody" Lude..=) Lude.<$> validateRequestBody
          ]
      )

instance Lude.ToPath CreateRequestValidator where
  toPath CreateRequestValidator' {..} =
    Lude.mconcat
      ["/restapis/", Lude.toBS restAPIId, "/requestvalidators"]

instance Lude.ToQuery CreateRequestValidator where
  toQuery = Lude.const Lude.mempty
