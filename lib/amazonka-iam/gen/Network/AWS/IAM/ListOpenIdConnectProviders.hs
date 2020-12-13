{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListOpenIdConnectProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the IAM OpenID Connect (OIDC) provider resource objects defined in the AWS account.
module Network.AWS.IAM.ListOpenIdConnectProviders
  ( -- * Creating a request
    ListOpenIdConnectProviders (..),
    mkListOpenIdConnectProviders,

    -- * Destructuring the response
    ListOpenIdConnectProvidersResponse (..),
    mkListOpenIdConnectProvidersResponse,

    -- ** Response lenses
    loicprsOpenIdConnectProviderList,
    loicprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOpenIdConnectProviders' smart constructor.
data ListOpenIdConnectProviders = ListOpenIdConnectProviders'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOpenIdConnectProviders' with the minimum fields required to make a request.
mkListOpenIdConnectProviders ::
  ListOpenIdConnectProviders
mkListOpenIdConnectProviders = ListOpenIdConnectProviders'

instance Lude.AWSRequest ListOpenIdConnectProviders where
  type
    Rs ListOpenIdConnectProviders =
      ListOpenIdConnectProvidersResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListOpenIDConnectProvidersResult"
      ( \s h x ->
          ListOpenIdConnectProvidersResponse'
            Lude.<$> ( x Lude..@? "OpenIDConnectProviderList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOpenIdConnectProviders where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListOpenIdConnectProviders where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOpenIdConnectProviders where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("ListOpenIDConnectProviders" :: Lude.ByteString),
            "Version" Lude.=: ("2010-05-08" :: Lude.ByteString)
          ]
      )

-- | Contains the response to a successful 'ListOpenIDConnectProviders' request.
--
-- /See:/ 'mkListOpenIdConnectProvidersResponse' smart constructor.
data ListOpenIdConnectProvidersResponse = ListOpenIdConnectProvidersResponse'
  { -- | The list of IAM OIDC provider resource objects defined in the AWS account.
    openIdConnectProviderList :: Lude.Maybe [OpenIdConnectProviderListEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOpenIdConnectProvidersResponse' with the minimum fields required to make a request.
--
-- * 'openIdConnectProviderList' - The list of IAM OIDC provider resource objects defined in the AWS account.
-- * 'responseStatus' - The response status code.
mkListOpenIdConnectProvidersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOpenIdConnectProvidersResponse
mkListOpenIdConnectProvidersResponse pResponseStatus_ =
  ListOpenIdConnectProvidersResponse'
    { openIdConnectProviderList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of IAM OIDC provider resource objects defined in the AWS account.
--
-- /Note:/ Consider using 'openIdConnectProviderList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loicprsOpenIdConnectProviderList :: Lens.Lens' ListOpenIdConnectProvidersResponse (Lude.Maybe [OpenIdConnectProviderListEntry])
loicprsOpenIdConnectProviderList = Lens.lens (openIdConnectProviderList :: ListOpenIdConnectProvidersResponse -> Lude.Maybe [OpenIdConnectProviderListEntry]) (\s a -> s {openIdConnectProviderList = a} :: ListOpenIdConnectProvidersResponse)
{-# DEPRECATED loicprsOpenIdConnectProviderList "Use generic-lens or generic-optics with 'openIdConnectProviderList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loicprsResponseStatus :: Lens.Lens' ListOpenIdConnectProvidersResponse Lude.Int
loicprsResponseStatus = Lens.lens (responseStatus :: ListOpenIdConnectProvidersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOpenIdConnectProvidersResponse)
{-# DEPRECATED loicprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
