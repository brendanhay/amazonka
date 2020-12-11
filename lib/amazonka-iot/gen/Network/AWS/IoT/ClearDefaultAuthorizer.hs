{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ClearDefaultAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears the default authorizer.
module Network.AWS.IoT.ClearDefaultAuthorizer
  ( -- * Creating a request
    ClearDefaultAuthorizer (..),
    mkClearDefaultAuthorizer,

    -- * Destructuring the response
    ClearDefaultAuthorizerResponse (..),
    mkClearDefaultAuthorizerResponse,

    -- ** Response lenses
    cdarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkClearDefaultAuthorizer' smart constructor.
data ClearDefaultAuthorizer = ClearDefaultAuthorizer'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClearDefaultAuthorizer' with the minimum fields required to make a request.
mkClearDefaultAuthorizer ::
  ClearDefaultAuthorizer
mkClearDefaultAuthorizer = ClearDefaultAuthorizer'

instance Lude.AWSRequest ClearDefaultAuthorizer where
  type Rs ClearDefaultAuthorizer = ClearDefaultAuthorizerResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ClearDefaultAuthorizerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ClearDefaultAuthorizer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ClearDefaultAuthorizer where
  toPath = Lude.const "/default-authorizer"

instance Lude.ToQuery ClearDefaultAuthorizer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkClearDefaultAuthorizerResponse' smart constructor.
newtype ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClearDefaultAuthorizerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkClearDefaultAuthorizerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ClearDefaultAuthorizerResponse
mkClearDefaultAuthorizerResponse pResponseStatus_ =
  ClearDefaultAuthorizerResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarsResponseStatus :: Lens.Lens' ClearDefaultAuthorizerResponse Lude.Int
cdarsResponseStatus = Lens.lens (responseStatus :: ClearDefaultAuthorizerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ClearDefaultAuthorizerResponse)
{-# DEPRECATED cdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
