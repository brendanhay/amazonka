{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteOpenIdConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenID Connect identity provider (IdP) resource object in IAM.
--
-- Deleting an IAM OIDC provider resource does not update any roles that reference the provider as a principal in their trust policies. Any attempt to assume a role that references a deleted provider fails.
-- This operation is idempotent; it does not fail or return an error if you call the operation for a provider that does not exist.
module Network.AWS.IAM.DeleteOpenIdConnectProvider
  ( -- * Creating a request
    DeleteOpenIdConnectProvider (..),
    mkDeleteOpenIdConnectProvider,

    -- ** Request lenses
    doicpOpenIdConnectProviderARN,

    -- * Destructuring the response
    DeleteOpenIdConnectProviderResponse (..),
    mkDeleteOpenIdConnectProviderResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteOpenIdConnectProvider' smart constructor.
newtype DeleteOpenIdConnectProvider = DeleteOpenIdConnectProvider'
  { -- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider resource object to delete. You can get a list of OpenID Connect provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
    openIdConnectProviderARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- * 'openIdConnectProviderARN' - The Amazon Resource Name (ARN) of the IAM OpenID Connect provider resource object to delete. You can get a list of OpenID Connect provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
mkDeleteOpenIdConnectProvider ::
  -- | 'openIdConnectProviderARN'
  Lude.Text ->
  DeleteOpenIdConnectProvider
mkDeleteOpenIdConnectProvider pOpenIdConnectProviderARN_ =
  DeleteOpenIdConnectProvider'
    { openIdConnectProviderARN =
        pOpenIdConnectProviderARN_
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider resource object to delete. You can get a list of OpenID Connect provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- /Note:/ Consider using 'openIdConnectProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doicpOpenIdConnectProviderARN :: Lens.Lens' DeleteOpenIdConnectProvider Lude.Text
doicpOpenIdConnectProviderARN = Lens.lens (openIdConnectProviderARN :: DeleteOpenIdConnectProvider -> Lude.Text) (\s a -> s {openIdConnectProviderARN = a} :: DeleteOpenIdConnectProvider)
{-# DEPRECATED doicpOpenIdConnectProviderARN "Use generic-lens or generic-optics with 'openIdConnectProviderARN' instead." #-}

instance Lude.AWSRequest DeleteOpenIdConnectProvider where
  type
    Rs DeleteOpenIdConnectProvider =
      DeleteOpenIdConnectProviderResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteOpenIdConnectProviderResponse'

instance Lude.ToHeaders DeleteOpenIdConnectProvider where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteOpenIdConnectProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOpenIdConnectProvider where
  toQuery DeleteOpenIdConnectProvider' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteOpenIDConnectProvider" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "OpenIDConnectProviderArn" Lude.=: openIdConnectProviderARN
      ]

-- | /See:/ 'mkDeleteOpenIdConnectProviderResponse' smart constructor.
data DeleteOpenIdConnectProviderResponse = DeleteOpenIdConnectProviderResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOpenIdConnectProviderResponse' with the minimum fields required to make a request.
mkDeleteOpenIdConnectProviderResponse ::
  DeleteOpenIdConnectProviderResponse
mkDeleteOpenIdConnectProviderResponse =
  DeleteOpenIdConnectProviderResponse'
