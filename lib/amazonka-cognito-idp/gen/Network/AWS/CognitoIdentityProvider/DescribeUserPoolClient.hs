{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Client method for returning the configuration information and metadata of the specified user pool app client.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
  ( -- * Creating a request
    DescribeUserPoolClient (..),
    mkDescribeUserPoolClient,

    -- ** Request lenses
    dupcuUserPoolId,
    dupcuClientId,

    -- * Destructuring the response
    DescribeUserPoolClientResponse (..),
    mkDescribeUserPoolClientResponse,

    -- ** Response lenses
    dupcrsUserPoolClient,
    dupcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to describe a user pool client.
--
-- /See:/ 'mkDescribeUserPoolClient' smart constructor.
data DescribeUserPoolClient = DescribeUserPoolClient'
  { userPoolId ::
      Lude.Text,
    clientId :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserPoolClient' with the minimum fields required to make a request.
--
-- * 'clientId' - The app client ID of the app associated with the user pool.
-- * 'userPoolId' - The user pool ID for the user pool you want to describe.
mkDescribeUserPoolClient ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  DescribeUserPoolClient
mkDescribeUserPoolClient pUserPoolId_ pClientId_ =
  DescribeUserPoolClient'
    { userPoolId = pUserPoolId_,
      clientId = pClientId_
    }

-- | The user pool ID for the user pool you want to describe.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcuUserPoolId :: Lens.Lens' DescribeUserPoolClient Lude.Text
dupcuUserPoolId = Lens.lens (userPoolId :: DescribeUserPoolClient -> Lude.Text) (\s a -> s {userPoolId = a} :: DescribeUserPoolClient)
{-# DEPRECATED dupcuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcuClientId :: Lens.Lens' DescribeUserPoolClient (Lude.Sensitive Lude.Text)
dupcuClientId = Lens.lens (clientId :: DescribeUserPoolClient -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: DescribeUserPoolClient)
{-# DEPRECATED dupcuClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

instance Lude.AWSRequest DescribeUserPoolClient where
  type Rs DescribeUserPoolClient = DescribeUserPoolClientResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserPoolClientResponse'
            Lude.<$> (x Lude..?> "UserPoolClient")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserPoolClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DescribeUserPoolClient" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserPoolClient where
  toJSON DescribeUserPoolClient' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("ClientId" Lude..= clientId)
          ]
      )

instance Lude.ToPath DescribeUserPoolClient where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserPoolClient where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server from a request to describe the user pool client.
--
-- /See:/ 'mkDescribeUserPoolClientResponse' smart constructor.
data DescribeUserPoolClientResponse = DescribeUserPoolClientResponse'
  { userPoolClient ::
      Lude.Maybe UserPoolClientType,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserPoolClientResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userPoolClient' - The user pool client from a server response to describe the user pool client.
mkDescribeUserPoolClientResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserPoolClientResponse
mkDescribeUserPoolClientResponse pResponseStatus_ =
  DescribeUserPoolClientResponse'
    { userPoolClient = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user pool client from a server response to describe the user pool client.
--
-- /Note:/ Consider using 'userPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcrsUserPoolClient :: Lens.Lens' DescribeUserPoolClientResponse (Lude.Maybe UserPoolClientType)
dupcrsUserPoolClient = Lens.lens (userPoolClient :: DescribeUserPoolClientResponse -> Lude.Maybe UserPoolClientType) (\s a -> s {userPoolClient = a} :: DescribeUserPoolClientResponse)
{-# DEPRECATED dupcrsUserPoolClient "Use generic-lens or generic-optics with 'userPoolClient' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcrsResponseStatus :: Lens.Lens' DescribeUserPoolClientResponse Lude.Int
dupcrsResponseStatus = Lens.lens (responseStatus :: DescribeUserPoolClientResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserPoolClientResponse)
{-# DEPRECATED dupcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
