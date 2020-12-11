{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a 'RequestValidator' of a given 'RestApi' .
module Network.AWS.APIGateway.GetRequestValidator
  ( -- * Creating a request
    GetRequestValidator (..),
    mkGetRequestValidator,

    -- ** Request lenses
    grvrRestAPIId,
    grvrRequestValidatorId,

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

-- | Gets a 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkGetRequestValidator' smart constructor.
data GetRequestValidator = GetRequestValidator'
  { restAPIId ::
      Lude.Text,
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

-- | Creates a value of 'GetRequestValidator' with the minimum fields required to make a request.
--
-- * 'requestValidatorId' - [Required] The identifier of the 'RequestValidator' to be retrieved.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetRequestValidator ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'requestValidatorId'
  Lude.Text ->
  GetRequestValidator
mkGetRequestValidator pRestAPIId_ pRequestValidatorId_ =
  GetRequestValidator'
    { restAPIId = pRestAPIId_,
      requestValidatorId = pRequestValidatorId_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrRestAPIId :: Lens.Lens' GetRequestValidator Lude.Text
grvrRestAPIId = Lens.lens (restAPIId :: GetRequestValidator -> Lude.Text) (\s a -> s {restAPIId = a} :: GetRequestValidator)
{-# DEPRECATED grvrRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The identifier of the 'RequestValidator' to be retrieved.
--
-- /Note:/ Consider using 'requestValidatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvrRequestValidatorId :: Lens.Lens' GetRequestValidator Lude.Text
grvrRequestValidatorId = Lens.lens (requestValidatorId :: GetRequestValidator -> Lude.Text) (\s a -> s {requestValidatorId = a} :: GetRequestValidator)
{-# DEPRECATED grvrRequestValidatorId "Use generic-lens or generic-optics with 'requestValidatorId' instead." #-}

instance Lude.AWSRequest GetRequestValidator where
  type Rs GetRequestValidator = RequestValidator
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetRequestValidator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetRequestValidator where
  toPath GetRequestValidator' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/requestvalidators/",
        Lude.toBS requestValidatorId
      ]

instance Lude.ToQuery GetRequestValidator where
  toQuery = Lude.const Lude.mempty
