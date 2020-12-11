{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an authorizer.
module Network.AWS.IoT.DescribeAuthorizer
  ( -- * Creating a request
    DescribeAuthorizer (..),
    mkDescribeAuthorizer,

    -- ** Request lenses
    daAuthorizerName,

    -- * Destructuring the response
    DescribeAuthorizerResponse (..),
    mkDescribeAuthorizerResponse,

    -- ** Response lenses
    darsAuthorizerDescription,
    darsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAuthorizer' smart constructor.
newtype DescribeAuthorizer = DescribeAuthorizer'
  { authorizerName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerName' - The name of the authorizer to describe.
mkDescribeAuthorizer ::
  -- | 'authorizerName'
  Lude.Text ->
  DescribeAuthorizer
mkDescribeAuthorizer pAuthorizerName_ =
  DescribeAuthorizer' {authorizerName = pAuthorizerName_}

-- | The name of the authorizer to describe.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAuthorizerName :: Lens.Lens' DescribeAuthorizer Lude.Text
daAuthorizerName = Lens.lens (authorizerName :: DescribeAuthorizer -> Lude.Text) (\s a -> s {authorizerName = a} :: DescribeAuthorizer)
{-# DEPRECATED daAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

instance Lude.AWSRequest DescribeAuthorizer where
  type Rs DescribeAuthorizer = DescribeAuthorizerResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAuthorizerResponse'
            Lude.<$> (x Lude..?> "authorizerDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAuthorizer where
  toPath DescribeAuthorizer' {..} =
    Lude.mconcat ["/authorizer/", Lude.toBS authorizerName]

instance Lude.ToQuery DescribeAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAuthorizerResponse' smart constructor.
data DescribeAuthorizerResponse = DescribeAuthorizerResponse'
  { authorizerDescription ::
      Lude.Maybe AuthorizerDescription,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'authorizerDescription' - The authorizer description.
-- * 'responseStatus' - The response status code.
mkDescribeAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAuthorizerResponse
mkDescribeAuthorizerResponse pResponseStatus_ =
  DescribeAuthorizerResponse'
    { authorizerDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The authorizer description.
--
-- /Note:/ Consider using 'authorizerDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAuthorizerDescription :: Lens.Lens' DescribeAuthorizerResponse (Lude.Maybe AuthorizerDescription)
darsAuthorizerDescription = Lens.lens (authorizerDescription :: DescribeAuthorizerResponse -> Lude.Maybe AuthorizerDescription) (\s a -> s {authorizerDescription = a} :: DescribeAuthorizerResponse)
{-# DEPRECATED darsAuthorizerDescription "Use generic-lens or generic-optics with 'authorizerDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAuthorizerResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAuthorizerResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
