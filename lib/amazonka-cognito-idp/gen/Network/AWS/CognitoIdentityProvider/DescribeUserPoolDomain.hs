{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a domain.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
  ( -- * Creating a request
    DescribeUserPoolDomain (..),
    mkDescribeUserPoolDomain,

    -- ** Request lenses
    dDomain,

    -- * Destructuring the response
    DescribeUserPoolDomainResponse (..),
    mkDescribeUserPoolDomainResponse,

    -- ** Response lenses
    drsDomainDescription,
    drsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUserPoolDomain' smart constructor.
newtype DescribeUserPoolDomain = DescribeUserPoolDomain'
  { -- | The domain string.
    domain :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserPoolDomain' with the minimum fields required to make a request.
--
-- * 'domain' - The domain string.
mkDescribeUserPoolDomain ::
  -- | 'domain'
  Lude.Text ->
  DescribeUserPoolDomain
mkDescribeUserPoolDomain pDomain_ =
  DescribeUserPoolDomain' {domain = pDomain_}

-- | The domain string.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomain :: Lens.Lens' DescribeUserPoolDomain Lude.Text
dDomain = Lens.lens (domain :: DescribeUserPoolDomain -> Lude.Text) (\s a -> s {domain = a} :: DescribeUserPoolDomain)
{-# DEPRECATED dDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Lude.AWSRequest DescribeUserPoolDomain where
  type Rs DescribeUserPoolDomain = DescribeUserPoolDomainResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserPoolDomainResponse'
            Lude.<$> (x Lude..?> "DomainDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserPoolDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DescribeUserPoolDomain" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserPoolDomain where
  toJSON DescribeUserPoolDomain' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Domain" Lude..= domain)])

instance Lude.ToPath DescribeUserPoolDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserPoolDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserPoolDomainResponse' smart constructor.
data DescribeUserPoolDomainResponse = DescribeUserPoolDomainResponse'
  { -- | A domain description object containing information about the domain.
    domainDescription :: Lude.Maybe DomainDescriptionType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainDescription' - A domain description object containing information about the domain.
-- * 'responseStatus' - The response status code.
mkDescribeUserPoolDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserPoolDomainResponse
mkDescribeUserPoolDomainResponse pResponseStatus_ =
  DescribeUserPoolDomainResponse'
    { domainDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A domain description object containing information about the domain.
--
-- /Note:/ Consider using 'domainDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDomainDescription :: Lens.Lens' DescribeUserPoolDomainResponse (Lude.Maybe DomainDescriptionType)
drsDomainDescription = Lens.lens (domainDescription :: DescribeUserPoolDomainResponse -> Lude.Maybe DomainDescriptionType) (\s a -> s {domainDescription = a} :: DescribeUserPoolDomainResponse)
{-# DEPRECATED drsDomainDescription "Use generic-lens or generic-optics with 'domainDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeUserPoolDomainResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeUserPoolDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserPoolDomainResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
