{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a domain for a user pool.
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
  ( -- * Creating a request
    DeleteUserPoolDomain (..),
    mkDeleteUserPoolDomain,

    -- ** Request lenses
    dupdUserPoolId,
    dupdDomain,

    -- * Destructuring the response
    DeleteUserPoolDomainResponse (..),
    mkDeleteUserPoolDomainResponse,

    -- ** Response lenses
    dupdrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserPoolDomain' smart constructor.
data DeleteUserPoolDomain = DeleteUserPoolDomain'
  { -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The domain string.
    domain :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPoolDomain' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
-- * 'domain' - The domain string.
mkDeleteUserPoolDomain ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'domain'
  Lude.Text ->
  DeleteUserPoolDomain
mkDeleteUserPoolDomain pUserPoolId_ pDomain_ =
  DeleteUserPoolDomain'
    { userPoolId = pUserPoolId_,
      domain = pDomain_
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupdUserPoolId :: Lens.Lens' DeleteUserPoolDomain Lude.Text
dupdUserPoolId = Lens.lens (userPoolId :: DeleteUserPoolDomain -> Lude.Text) (\s a -> s {userPoolId = a} :: DeleteUserPoolDomain)
{-# DEPRECATED dupdUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The domain string.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupdDomain :: Lens.Lens' DeleteUserPoolDomain Lude.Text
dupdDomain = Lens.lens (domain :: DeleteUserPoolDomain -> Lude.Text) (\s a -> s {domain = a} :: DeleteUserPoolDomain)
{-# DEPRECATED dupdDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Lude.AWSRequest DeleteUserPoolDomain where
  type Rs DeleteUserPoolDomain = DeleteUserPoolDomainResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUserPoolDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUserPoolDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteUserPoolDomain" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUserPoolDomain where
  toJSON DeleteUserPoolDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Domain" Lude..= domain)
          ]
      )

instance Lude.ToPath DeleteUserPoolDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserPoolDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserPoolDomainResponse' smart constructor.
newtype DeleteUserPoolDomainResponse = DeleteUserPoolDomainResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteUserPoolDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUserPoolDomainResponse
mkDeleteUserPoolDomainResponse pResponseStatus_ =
  DeleteUserPoolDomainResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupdrsResponseStatus :: Lens.Lens' DeleteUserPoolDomainResponse Lude.Int
dupdrsResponseStatus = Lens.lens (responseStatus :: DeleteUserPoolDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserPoolDomainResponse)
{-# DEPRECATED dupdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
