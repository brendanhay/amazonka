{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeDefaultAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default authorizer.
module Network.AWS.IoT.DescribeDefaultAuthorizer
  ( -- * Creating a request
    DescribeDefaultAuthorizer (..),
    mkDescribeDefaultAuthorizer,

    -- * Destructuring the response
    DescribeDefaultAuthorizerResponse (..),
    mkDescribeDefaultAuthorizerResponse,

    -- ** Response lenses
    ddarsAuthorizerDescription,
    ddarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDefaultAuthorizer' smart constructor.
data DescribeDefaultAuthorizer = DescribeDefaultAuthorizer'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDefaultAuthorizer' with the minimum fields required to make a request.
mkDescribeDefaultAuthorizer ::
  DescribeDefaultAuthorizer
mkDescribeDefaultAuthorizer = DescribeDefaultAuthorizer'

instance Lude.AWSRequest DescribeDefaultAuthorizer where
  type
    Rs DescribeDefaultAuthorizer =
      DescribeDefaultAuthorizerResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDefaultAuthorizerResponse'
            Lude.<$> (x Lude..?> "authorizerDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDefaultAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDefaultAuthorizer where
  toPath = Lude.const "/default-authorizer"

instance Lude.ToQuery DescribeDefaultAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDefaultAuthorizerResponse' smart constructor.
data DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse'
  { -- | The default authorizer's description.
    authorizerDescription :: Lude.Maybe AuthorizerDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDefaultAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'authorizerDescription' - The default authorizer's description.
-- * 'responseStatus' - The response status code.
mkDescribeDefaultAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDefaultAuthorizerResponse
mkDescribeDefaultAuthorizerResponse pResponseStatus_ =
  DescribeDefaultAuthorizerResponse'
    { authorizerDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The default authorizer's description.
--
-- /Note:/ Consider using 'authorizerDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarsAuthorizerDescription :: Lens.Lens' DescribeDefaultAuthorizerResponse (Lude.Maybe AuthorizerDescription)
ddarsAuthorizerDescription = Lens.lens (authorizerDescription :: DescribeDefaultAuthorizerResponse -> Lude.Maybe AuthorizerDescription) (\s a -> s {authorizerDescription = a} :: DescribeDefaultAuthorizerResponse)
{-# DEPRECATED ddarsAuthorizerDescription "Use generic-lens or generic-optics with 'authorizerDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarsResponseStatus :: Lens.Lens' DescribeDefaultAuthorizerResponse Lude.Int
ddarsResponseStatus = Lens.lens (responseStatus :: DescribeDefaultAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDefaultAuthorizerResponse)
{-# DEPRECATED ddarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
