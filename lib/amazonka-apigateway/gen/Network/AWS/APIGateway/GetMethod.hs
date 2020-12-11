{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Method' resource.
module Network.AWS.APIGateway.GetMethod
  ( -- * Creating a request
    GetMethod (..),
    mkGetMethod,

    -- ** Request lenses
    gmmRestAPIId,
    gmmResourceId,
    gmmHttpMethod,

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

-- | Request to describe an existing 'Method' resource.
--
-- /See:/ 'mkGetMethod' smart constructor.
data GetMethod = GetMethod'
  { restAPIId :: Lude.Text,
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

-- | Creates a value of 'GetMethod' with the minimum fields required to make a request.
--
-- * 'httpMethod' - [Required] Specifies the method request's HTTP method type.
-- * 'resourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetMethod ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  GetMethod
mkGetMethod pRestAPIId_ pResourceId_ pHttpMethod_ =
  GetMethod'
    { restAPIId = pRestAPIId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmmRestAPIId :: Lens.Lens' GetMethod Lude.Text
gmmRestAPIId = Lens.lens (restAPIId :: GetMethod -> Lude.Text) (\s a -> s {restAPIId = a} :: GetMethod)
{-# DEPRECATED gmmRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmmResourceId :: Lens.Lens' GetMethod Lude.Text
gmmResourceId = Lens.lens (resourceId :: GetMethod -> Lude.Text) (\s a -> s {resourceId = a} :: GetMethod)
{-# DEPRECATED gmmResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies the method request's HTTP method type.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmmHttpMethod :: Lens.Lens' GetMethod Lude.Text
gmmHttpMethod = Lens.lens (httpMethod :: GetMethod -> Lude.Text) (\s a -> s {httpMethod = a} :: GetMethod)
{-# DEPRECATED gmmHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

instance Lude.AWSRequest GetMethod where
  type Rs GetMethod = Method
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetMethod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetMethod where
  toPath GetMethod' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod
      ]

instance Lude.ToQuery GetMethod where
  toQuery = Lude.const Lude.mempty
