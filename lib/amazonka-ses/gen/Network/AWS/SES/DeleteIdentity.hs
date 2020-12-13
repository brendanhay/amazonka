{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified identity (an email address or a domain) from the list of verified identities.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteIdentity
  ( -- * Creating a request
    DeleteIdentity (..),
    mkDeleteIdentity,

    -- ** Request lenses
    diIdentity,

    -- * Destructuring the response
    DeleteIdentityResponse (..),
    mkDeleteIdentityResponse,

    -- ** Response lenses
    dirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete one of your Amazon SES identities (an email address or domain).
--
-- /See:/ 'mkDeleteIdentity' smart constructor.
newtype DeleteIdentity = DeleteIdentity'
  { -- | The identity to be removed from the list of identities for the AWS Account.
    identity :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentity' with the minimum fields required to make a request.
--
-- * 'identity' - The identity to be removed from the list of identities for the AWS Account.
mkDeleteIdentity ::
  -- | 'identity'
  Lude.Text ->
  DeleteIdentity
mkDeleteIdentity pIdentity_ =
  DeleteIdentity' {identity = pIdentity_}

-- | The identity to be removed from the list of identities for the AWS Account.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIdentity :: Lens.Lens' DeleteIdentity Lude.Text
diIdentity = Lens.lens (identity :: DeleteIdentity -> Lude.Text) (\s a -> s {identity = a} :: DeleteIdentity)
{-# DEPRECATED diIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Lude.AWSRequest DeleteIdentity where
  type Rs DeleteIdentity = DeleteIdentityResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteIdentityResult"
      ( \s h x ->
          DeleteIdentityResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteIdentity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIdentity where
  toQuery DeleteIdentity' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteIdentity" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identity" Lude.=: identity
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteIdentityResponse' smart constructor.
newtype DeleteIdentityResponse = DeleteIdentityResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentityResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteIdentityResponse
mkDeleteIdentityResponse pResponseStatus_ =
  DeleteIdentityResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteIdentityResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteIdentityResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
