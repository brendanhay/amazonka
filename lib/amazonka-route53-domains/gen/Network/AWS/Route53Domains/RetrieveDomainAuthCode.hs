{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the AuthCode for the domain. To transfer a domain to another registrar, you provide this value to the new registrar.
module Network.AWS.Route53Domains.RetrieveDomainAuthCode
  ( -- * Creating a request
    RetrieveDomainAuthCode (..),
    mkRetrieveDomainAuthCode,

    -- ** Request lenses
    rdacDomainName,

    -- * Destructuring the response
    RetrieveDomainAuthCodeResponse (..),
    mkRetrieveDomainAuthCodeResponse,

    -- ** Response lenses
    rdacrsAuthCode,
    rdacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | A request for the authorization code for the specified domain. To transfer a domain to another registrar, you provide this value to the new registrar.
--
-- /See:/ 'mkRetrieveDomainAuthCode' smart constructor.
newtype RetrieveDomainAuthCode = RetrieveDomainAuthCode'
  { -- | The name of the domain that you want to get an authorization code for.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveDomainAuthCode' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to get an authorization code for.
mkRetrieveDomainAuthCode ::
  -- | 'domainName'
  Lude.Text ->
  RetrieveDomainAuthCode
mkRetrieveDomainAuthCode pDomainName_ =
  RetrieveDomainAuthCode' {domainName = pDomainName_}

-- | The name of the domain that you want to get an authorization code for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdacDomainName :: Lens.Lens' RetrieveDomainAuthCode Lude.Text
rdacDomainName = Lens.lens (domainName :: RetrieveDomainAuthCode -> Lude.Text) (\s a -> s {domainName = a} :: RetrieveDomainAuthCode)
{-# DEPRECATED rdacDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest RetrieveDomainAuthCode where
  type Rs RetrieveDomainAuthCode = RetrieveDomainAuthCodeResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RetrieveDomainAuthCodeResponse'
            Lude.<$> (x Lude..:> "AuthCode") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RetrieveDomainAuthCode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.RetrieveDomainAuthCode" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RetrieveDomainAuthCode where
  toJSON RetrieveDomainAuthCode' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath RetrieveDomainAuthCode where
  toPath = Lude.const "/"

instance Lude.ToQuery RetrieveDomainAuthCode where
  toQuery = Lude.const Lude.mempty

-- | The RetrieveDomainAuthCode response includes the following element.
--
-- /See:/ 'mkRetrieveDomainAuthCodeResponse' smart constructor.
data RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse'
  { -- | The authorization code for the domain.
    authCode :: Lude.Sensitive Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveDomainAuthCodeResponse' with the minimum fields required to make a request.
--
-- * 'authCode' - The authorization code for the domain.
-- * 'responseStatus' - The response status code.
mkRetrieveDomainAuthCodeResponse ::
  -- | 'authCode'
  Lude.Sensitive Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  RetrieveDomainAuthCodeResponse
mkRetrieveDomainAuthCodeResponse pAuthCode_ pResponseStatus_ =
  RetrieveDomainAuthCodeResponse'
    { authCode = pAuthCode_,
      responseStatus = pResponseStatus_
    }

-- | The authorization code for the domain.
--
-- /Note:/ Consider using 'authCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdacrsAuthCode :: Lens.Lens' RetrieveDomainAuthCodeResponse (Lude.Sensitive Lude.Text)
rdacrsAuthCode = Lens.lens (authCode :: RetrieveDomainAuthCodeResponse -> Lude.Sensitive Lude.Text) (\s a -> s {authCode = a} :: RetrieveDomainAuthCodeResponse)
{-# DEPRECATED rdacrsAuthCode "Use generic-lens or generic-optics with 'authCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdacrsResponseStatus :: Lens.Lens' RetrieveDomainAuthCodeResponse Lude.Int
rdacrsResponseStatus = Lens.lens (responseStatus :: RetrieveDomainAuthCodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RetrieveDomainAuthCodeResponse)
{-# DEPRECATED rdacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
