{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration information and metadata of the specified user pool.
module Network.AWS.CognitoIdentityProvider.DescribeUserPool
  ( -- * Creating a request
    DescribeUserPool (..),
    mkDescribeUserPool,

    -- ** Request lenses
    dupfUserPoolId,

    -- * Destructuring the response
    DescribeUserPoolResponse (..),
    mkDescribeUserPoolResponse,

    -- ** Response lenses
    duprsUserPool,
    duprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to describe the user pool.
--
-- /See:/ 'mkDescribeUserPool' smart constructor.
newtype DescribeUserPool = DescribeUserPool'
  { -- | The user pool ID for the user pool you want to describe.
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserPool' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool you want to describe.
mkDescribeUserPool ::
  -- | 'userPoolId'
  Lude.Text ->
  DescribeUserPool
mkDescribeUserPool pUserPoolId_ =
  DescribeUserPool' {userPoolId = pUserPoolId_}

-- | The user pool ID for the user pool you want to describe.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupfUserPoolId :: Lens.Lens' DescribeUserPool Lude.Text
dupfUserPoolId = Lens.lens (userPoolId :: DescribeUserPool -> Lude.Text) (\s a -> s {userPoolId = a} :: DescribeUserPool)
{-# DEPRECATED dupfUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest DescribeUserPool where
  type Rs DescribeUserPool = DescribeUserPoolResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserPoolResponse'
            Lude.<$> (x Lude..?> "UserPool") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DescribeUserPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserPool where
  toJSON DescribeUserPool' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("UserPoolId" Lude..= userPoolId)])

instance Lude.ToPath DescribeUserPool where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserPool where
  toQuery = Lude.const Lude.mempty

-- | Represents the response to describe the user pool.
--
-- /See:/ 'mkDescribeUserPoolResponse' smart constructor.
data DescribeUserPoolResponse = DescribeUserPoolResponse'
  { -- | The container of metadata returned by the server to describe the pool.
    userPool :: Lude.Maybe UserPoolType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserPoolResponse' with the minimum fields required to make a request.
--
-- * 'userPool' - The container of metadata returned by the server to describe the pool.
-- * 'responseStatus' - The response status code.
mkDescribeUserPoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserPoolResponse
mkDescribeUserPoolResponse pResponseStatus_ =
  DescribeUserPoolResponse'
    { userPool = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The container of metadata returned by the server to describe the pool.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsUserPool :: Lens.Lens' DescribeUserPoolResponse (Lude.Maybe UserPoolType)
duprsUserPool = Lens.lens (userPool :: DescribeUserPoolResponse -> Lude.Maybe UserPoolType) (\s a -> s {userPool = a} :: DescribeUserPoolResponse)
{-# DEPRECATED duprsUserPool "Use generic-lens or generic-optics with 'userPool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsResponseStatus :: Lens.Lens' DescribeUserPoolResponse Lude.Int
duprsResponseStatus = Lens.lens (responseStatus :: DescribeUserPoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserPoolResponse)
{-# DEPRECATED duprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
