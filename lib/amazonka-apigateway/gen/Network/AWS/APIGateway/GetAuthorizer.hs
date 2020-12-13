{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Authorizer' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizer.html AWS CLI>
module Network.AWS.APIGateway.GetAuthorizer
  ( -- * Creating a request
    GetAuthorizer (..),
    mkGetAuthorizer,

    -- ** Request lenses
    gafAuthorizerId,
    gafRestAPIId,

    -- * Destructuring the response
    Authorizer (..),
    mkAuthorizer,

    -- ** Response lenses
    aAuthorizerURI,
    aIdentityValidationExpression,
    aProviderARNs,
    aName,
    aId,
    aAuthorizerResultTtlInSeconds,
    aAuthType,
    aType,
    aIdentitySource,
    aAuthorizerCredentials,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to describe an existing 'Authorizer' resource.
--
-- /See:/ 'mkGetAuthorizer' smart constructor.
data GetAuthorizer = GetAuthorizer'
  { -- | [Required] The identifier of the 'Authorizer' resource.
    authorizerId :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerId' - [Required] The identifier of the 'Authorizer' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetAuthorizer ::
  -- | 'authorizerId'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  GetAuthorizer
mkGetAuthorizer pAuthorizerId_ pRestAPIId_ =
  GetAuthorizer'
    { authorizerId = pAuthorizerId_,
      restAPIId = pRestAPIId_
    }

-- | [Required] The identifier of the 'Authorizer' resource.
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gafAuthorizerId :: Lens.Lens' GetAuthorizer Lude.Text
gafAuthorizerId = Lens.lens (authorizerId :: GetAuthorizer -> Lude.Text) (\s a -> s {authorizerId = a} :: GetAuthorizer)
{-# DEPRECATED gafAuthorizerId "Use generic-lens or generic-optics with 'authorizerId' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gafRestAPIId :: Lens.Lens' GetAuthorizer Lude.Text
gafRestAPIId = Lens.lens (restAPIId :: GetAuthorizer -> Lude.Text) (\s a -> s {restAPIId = a} :: GetAuthorizer)
{-# DEPRECATED gafRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest GetAuthorizer where
  type Rs GetAuthorizer = Authorizer
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetAuthorizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetAuthorizer where
  toPath GetAuthorizer' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/authorizers/",
        Lude.toBS authorizerId
      ]

instance Lude.ToQuery GetAuthorizer where
  toQuery = Lude.const Lude.mempty
