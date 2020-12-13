{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListSAMLProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SAML provider resource objects defined in IAM in the account.
module Network.AWS.IAM.ListSAMLProviders
  ( -- * Creating a request
    ListSAMLProviders (..),
    mkListSAMLProviders,

    -- * Destructuring the response
    ListSAMLProvidersResponse (..),
    mkListSAMLProvidersResponse,

    -- ** Response lenses
    lsamlprsSAMLProviderList,
    lsamlprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSAMLProviders' smart constructor.
data ListSAMLProviders = ListSAMLProviders'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSAMLProviders' with the minimum fields required to make a request.
mkListSAMLProviders ::
  ListSAMLProviders
mkListSAMLProviders = ListSAMLProviders'

instance Lude.AWSRequest ListSAMLProviders where
  type Rs ListSAMLProviders = ListSAMLProvidersResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListSAMLProvidersResult"
      ( \s h x ->
          ListSAMLProvidersResponse'
            Lude.<$> ( x Lude..@? "SAMLProviderList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSAMLProviders where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSAMLProviders where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSAMLProviders where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("ListSAMLProviders" :: Lude.ByteString),
            "Version" Lude.=: ("2010-05-08" :: Lude.ByteString)
          ]
      )

-- | Contains the response to a successful 'ListSAMLProviders' request.
--
-- /See:/ 'mkListSAMLProvidersResponse' smart constructor.
data ListSAMLProvidersResponse = ListSAMLProvidersResponse'
  { -- | The list of SAML provider resource objects defined in IAM for this AWS account.
    sAMLProviderList :: Lude.Maybe [SAMLProviderListEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSAMLProvidersResponse' with the minimum fields required to make a request.
--
-- * 'sAMLProviderList' - The list of SAML provider resource objects defined in IAM for this AWS account.
-- * 'responseStatus' - The response status code.
mkListSAMLProvidersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSAMLProvidersResponse
mkListSAMLProvidersResponse pResponseStatus_ =
  ListSAMLProvidersResponse'
    { sAMLProviderList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of SAML provider resource objects defined in IAM for this AWS account.
--
-- /Note:/ Consider using 'sAMLProviderList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsamlprsSAMLProviderList :: Lens.Lens' ListSAMLProvidersResponse (Lude.Maybe [SAMLProviderListEntry])
lsamlprsSAMLProviderList = Lens.lens (sAMLProviderList :: ListSAMLProvidersResponse -> Lude.Maybe [SAMLProviderListEntry]) (\s a -> s {sAMLProviderList = a} :: ListSAMLProvidersResponse)
{-# DEPRECATED lsamlprsSAMLProviderList "Use generic-lens or generic-optics with 'sAMLProviderList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsamlprsResponseStatus :: Lens.Lens' ListSAMLProvidersResponse Lude.Int
lsamlprsResponseStatus = Lens.lens (responseStatus :: ListSAMLProvidersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSAMLProvidersResponse)
{-# DEPRECATED lsamlprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
