{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified client ID (also known as audience) from the list of client IDs registered for the specified IAM OpenID Connect (OIDC) provider resource object.
--
-- This operation is idempotent; it does not fail or return an error if you try to remove a client ID that does not exist.
module Network.AWS.IAM.RemoveClientIdFromOpenIdConnectProvider
  ( -- * Creating a request
    RemoveClientIdFromOpenIdConnectProvider (..),
    mkRemoveClientIdFromOpenIdConnectProvider,

    -- ** Request lenses
    rcifoicpOpenIdConnectProviderARN,
    rcifoicpClientId,

    -- * Destructuring the response
    RemoveClientIdFromOpenIdConnectProviderResponse (..),
    mkRemoveClientIdFromOpenIdConnectProviderResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveClientIdFromOpenIdConnectProvider' smart constructor.
data RemoveClientIdFromOpenIdConnectProvider = RemoveClientIdFromOpenIdConnectProvider'
  { openIdConnectProviderARN ::
      Lude.Text,
    clientId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveClientIdFromOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- * 'clientId' - The client ID (also known as audience) to remove from the IAM OIDC provider resource. For more information about client IDs, see 'CreateOpenIDConnectProvider' .
-- * 'openIdConnectProviderARN' - The Amazon Resource Name (ARN) of the IAM OIDC provider resource to remove the client ID from. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkRemoveClientIdFromOpenIdConnectProvider ::
  -- | 'openIdConnectProviderARN'
  Lude.Text ->
  -- | 'clientId'
  Lude.Text ->
  RemoveClientIdFromOpenIdConnectProvider
mkRemoveClientIdFromOpenIdConnectProvider
  pOpenIdConnectProviderARN_
  pClientId_ =
    RemoveClientIdFromOpenIdConnectProvider'
      { openIdConnectProviderARN =
          pOpenIdConnectProviderARN_,
        clientId = pClientId_
      }

-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource to remove the client ID from. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'openIdConnectProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcifoicpOpenIdConnectProviderARN :: Lens.Lens' RemoveClientIdFromOpenIdConnectProvider Lude.Text
rcifoicpOpenIdConnectProviderARN = Lens.lens (openIdConnectProviderARN :: RemoveClientIdFromOpenIdConnectProvider -> Lude.Text) (\s a -> s {openIdConnectProviderARN = a} :: RemoveClientIdFromOpenIdConnectProvider)
{-# DEPRECATED rcifoicpOpenIdConnectProviderARN "Use generic-lens or generic-optics with 'openIdConnectProviderARN' instead." #-}

-- | The client ID (also known as audience) to remove from the IAM OIDC provider resource. For more information about client IDs, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcifoicpClientId :: Lens.Lens' RemoveClientIdFromOpenIdConnectProvider Lude.Text
rcifoicpClientId = Lens.lens (clientId :: RemoveClientIdFromOpenIdConnectProvider -> Lude.Text) (\s a -> s {clientId = a} :: RemoveClientIdFromOpenIdConnectProvider)
{-# DEPRECATED rcifoicpClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

instance Lude.AWSRequest RemoveClientIdFromOpenIdConnectProvider where
  type
    Rs RemoveClientIdFromOpenIdConnectProvider =
      RemoveClientIdFromOpenIdConnectProviderResponse
  request = Req.postQuery iamService
  response =
    Res.receiveNull RemoveClientIdFromOpenIdConnectProviderResponse'

instance Lude.ToHeaders RemoveClientIdFromOpenIdConnectProvider where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveClientIdFromOpenIdConnectProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveClientIdFromOpenIdConnectProvider where
  toQuery RemoveClientIdFromOpenIdConnectProvider' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RemoveClientIDFromOpenIDConnectProvider" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "OpenIDConnectProviderArn" Lude.=: openIdConnectProviderARN,
        "ClientID" Lude.=: clientId
      ]

-- | /See:/ 'mkRemoveClientIdFromOpenIdConnectProviderResponse' smart constructor.
data RemoveClientIdFromOpenIdConnectProviderResponse = RemoveClientIdFromOpenIdConnectProviderResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'RemoveClientIdFromOpenIdConnectProviderResponse' with the minimum fields required to make a request.
mkRemoveClientIdFromOpenIdConnectProviderResponse ::
  RemoveClientIdFromOpenIdConnectProviderResponse
mkRemoveClientIdFromOpenIdConnectProviderResponse =
  RemoveClientIdFromOpenIdConnectProviderResponse'
