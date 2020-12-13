{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'Authorizer' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/delete-authorizer.html AWS CLI>
module Network.AWS.APIGateway.DeleteAuthorizer
  ( -- * Creating a request
    DeleteAuthorizer (..),
    mkDeleteAuthorizer,

    -- ** Request lenses
    daAuthorizerId,
    daRestAPIId,

    -- * Destructuring the response
    DeleteAuthorizerResponse (..),
    mkDeleteAuthorizerResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete an existing 'Authorizer' resource.
--
-- /See:/ 'mkDeleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { -- | [Required] The identifier of the 'Authorizer' resource.
    authorizerId :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerId' - [Required] The identifier of the 'Authorizer' resource.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteAuthorizer ::
  -- | 'authorizerId'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  DeleteAuthorizer
mkDeleteAuthorizer pAuthorizerId_ pRestAPIId_ =
  DeleteAuthorizer'
    { authorizerId = pAuthorizerId_,
      restAPIId = pRestAPIId_
    }

-- | [Required] The identifier of the 'Authorizer' resource.
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAuthorizerId :: Lens.Lens' DeleteAuthorizer Lude.Text
daAuthorizerId = Lens.lens (authorizerId :: DeleteAuthorizer -> Lude.Text) (\s a -> s {authorizerId = a} :: DeleteAuthorizer)
{-# DEPRECATED daAuthorizerId "Use generic-lens or generic-optics with 'authorizerId' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daRestAPIId :: Lens.Lens' DeleteAuthorizer Lude.Text
daRestAPIId = Lens.lens (restAPIId :: DeleteAuthorizer -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteAuthorizer)
{-# DEPRECATED daRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest DeleteAuthorizer where
  type Rs DeleteAuthorizer = DeleteAuthorizerResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteAuthorizerResponse'

instance Lude.ToHeaders DeleteAuthorizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteAuthorizer where
  toPath DeleteAuthorizer' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/authorizers/",
        Lude.toBS authorizerId
      ]

instance Lude.ToQuery DeleteAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAuthorizerResponse' smart constructor.
data DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAuthorizerResponse' with the minimum fields required to make a request.
mkDeleteAuthorizerResponse ::
  DeleteAuthorizerResponse
mkDeleteAuthorizerResponse = DeleteAuthorizerResponse'
