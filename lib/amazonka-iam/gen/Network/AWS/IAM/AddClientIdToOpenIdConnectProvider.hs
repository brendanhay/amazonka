{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddClientIdToOpenIdConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new client ID (also known as audience) to the list of client IDs already registered for the specified IAM OpenID Connect (OIDC) provider resource.
--
-- This operation is idempotent; it does not fail or return an error if you add an existing client ID to the provider.
module Network.AWS.IAM.AddClientIdToOpenIdConnectProvider
  ( -- * Creating a request
    AddClientIdToOpenIdConnectProvider (..),
    mkAddClientIdToOpenIdConnectProvider,

    -- ** Request lenses
    acitoicpOpenIdConnectProviderARN,
    acitoicpClientId,

    -- * Destructuring the response
    AddClientIdToOpenIdConnectProviderResponse (..),
    mkAddClientIdToOpenIdConnectProviderResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddClientIdToOpenIdConnectProvider' smart constructor.
data AddClientIdToOpenIdConnectProvider = AddClientIdToOpenIdConnectProvider'
  { openIdConnectProviderARN ::
      Lude.Text,
    clientId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddClientIdToOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- * 'clientId' - The client ID (also known as audience) to add to the IAM OpenID Connect provider resource.
-- * 'openIdConnectProviderARN' - The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider resource to add the client ID to. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
mkAddClientIdToOpenIdConnectProvider ::
  -- | 'openIdConnectProviderARN'
  Lude.Text ->
  -- | 'clientId'
  Lude.Text ->
  AddClientIdToOpenIdConnectProvider
mkAddClientIdToOpenIdConnectProvider
  pOpenIdConnectProviderARN_
  pClientId_ =
    AddClientIdToOpenIdConnectProvider'
      { openIdConnectProviderARN =
          pOpenIdConnectProviderARN_,
        clientId = pClientId_
      }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider resource to add the client ID to. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- /Note:/ Consider using 'openIdConnectProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitoicpOpenIdConnectProviderARN :: Lens.Lens' AddClientIdToOpenIdConnectProvider Lude.Text
acitoicpOpenIdConnectProviderARN = Lens.lens (openIdConnectProviderARN :: AddClientIdToOpenIdConnectProvider -> Lude.Text) (\s a -> s {openIdConnectProviderARN = a} :: AddClientIdToOpenIdConnectProvider)
{-# DEPRECATED acitoicpOpenIdConnectProviderARN "Use generic-lens or generic-optics with 'openIdConnectProviderARN' instead." #-}

-- | The client ID (also known as audience) to add to the IAM OpenID Connect provider resource.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitoicpClientId :: Lens.Lens' AddClientIdToOpenIdConnectProvider Lude.Text
acitoicpClientId = Lens.lens (clientId :: AddClientIdToOpenIdConnectProvider -> Lude.Text) (\s a -> s {clientId = a} :: AddClientIdToOpenIdConnectProvider)
{-# DEPRECATED acitoicpClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

instance Lude.AWSRequest AddClientIdToOpenIdConnectProvider where
  type
    Rs AddClientIdToOpenIdConnectProvider =
      AddClientIdToOpenIdConnectProviderResponse
  request = Req.postQuery iamService
  response =
    Res.receiveNull AddClientIdToOpenIdConnectProviderResponse'

instance Lude.ToHeaders AddClientIdToOpenIdConnectProvider where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddClientIdToOpenIdConnectProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery AddClientIdToOpenIdConnectProvider where
  toQuery AddClientIdToOpenIdConnectProvider' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AddClientIDToOpenIDConnectProvider" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "OpenIDConnectProviderArn" Lude.=: openIdConnectProviderARN,
        "ClientID" Lude.=: clientId
      ]

-- | /See:/ 'mkAddClientIdToOpenIdConnectProviderResponse' smart constructor.
data AddClientIdToOpenIdConnectProviderResponse = AddClientIdToOpenIdConnectProviderResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddClientIdToOpenIdConnectProviderResponse' with the minimum fields required to make a request.
mkAddClientIdToOpenIdConnectProviderResponse ::
  AddClientIdToOpenIdConnectProviderResponse
mkAddClientIdToOpenIdConnectProviderResponse =
  AddClientIdToOpenIdConnectProviderResponse'
