{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the private certificate authorities that you created by using the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action.
--
-- This operation returns paginated results.
module Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
  ( -- * Creating a request
    ListCertificateAuthorities (..),
    mkListCertificateAuthorities,

    -- ** Request lenses
    lcaNextToken,
    lcaResourceOwner,
    lcaMaxResults,

    -- * Destructuring the response
    ListCertificateAuthoritiesResponse (..),
    mkListCertificateAuthoritiesResponse,

    -- ** Response lenses
    lcarsCertificateAuthorities,
    lcarsNextToken,
    lcarsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCertificateAuthorities' smart constructor.
data ListCertificateAuthorities = ListCertificateAuthorities'
  { -- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Use this parameter to filter the returned set of certificate authorities based on their owner. The default is SELF.
    resourceOwner :: Lude.Maybe ResourceOwner,
    -- | Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCertificateAuthorities' with the minimum fields required to make a request.
--
-- * 'nextToken' - Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
-- * 'resourceOwner' - Use this parameter to filter the returned set of certificate authorities based on their owner. The default is SELF.
-- * 'maxResults' - Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
mkListCertificateAuthorities ::
  ListCertificateAuthorities
mkListCertificateAuthorities =
  ListCertificateAuthorities'
    { nextToken = Lude.Nothing,
      resourceOwner = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaNextToken :: Lens.Lens' ListCertificateAuthorities (Lude.Maybe Lude.Text)
lcaNextToken = Lens.lens (nextToken :: ListCertificateAuthorities -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCertificateAuthorities)
{-# DEPRECATED lcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Use this parameter to filter the returned set of certificate authorities based on their owner. The default is SELF.
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaResourceOwner :: Lens.Lens' ListCertificateAuthorities (Lude.Maybe ResourceOwner)
lcaResourceOwner = Lens.lens (resourceOwner :: ListCertificateAuthorities -> Lude.Maybe ResourceOwner) (\s a -> s {resourceOwner = a} :: ListCertificateAuthorities)
{-# DEPRECATED lcaResourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead." #-}

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcaMaxResults :: Lens.Lens' ListCertificateAuthorities (Lude.Maybe Lude.Natural)
lcaMaxResults = Lens.lens (maxResults :: ListCertificateAuthorities -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCertificateAuthorities)
{-# DEPRECATED lcaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCertificateAuthorities where
  page rq rs
    | Page.stop (rs Lens.^. lcarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcarsCertificateAuthorities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcaNextToken Lens..~ rs Lens.^. lcarsNextToken

instance Lude.AWSRequest ListCertificateAuthorities where
  type
    Rs ListCertificateAuthorities =
      ListCertificateAuthoritiesResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCertificateAuthoritiesResponse'
            Lude.<$> (x Lude..?> "CertificateAuthorities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCertificateAuthorities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.ListCertificateAuthorities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCertificateAuthorities where
  toJSON ListCertificateAuthorities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ResourceOwner" Lude..=) Lude.<$> resourceOwner,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCertificateAuthorities where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCertificateAuthorities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCertificateAuthoritiesResponse' smart constructor.
data ListCertificateAuthoritiesResponse = ListCertificateAuthoritiesResponse'
  { -- | Summary information about each certificate authority you have created.
    certificateAuthorities :: Lude.Maybe [CertificateAuthority],
    -- | When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCertificateAuthoritiesResponse' with the minimum fields required to make a request.
--
-- * 'certificateAuthorities' - Summary information about each certificate authority you have created.
-- * 'nextToken' - When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
mkListCertificateAuthoritiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCertificateAuthoritiesResponse
mkListCertificateAuthoritiesResponse pResponseStatus_ =
  ListCertificateAuthoritiesResponse'
    { certificateAuthorities =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Summary information about each certificate authority you have created.
--
-- /Note:/ Consider using 'certificateAuthorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarsCertificateAuthorities :: Lens.Lens' ListCertificateAuthoritiesResponse (Lude.Maybe [CertificateAuthority])
lcarsCertificateAuthorities = Lens.lens (certificateAuthorities :: ListCertificateAuthoritiesResponse -> Lude.Maybe [CertificateAuthority]) (\s a -> s {certificateAuthorities = a} :: ListCertificateAuthoritiesResponse)
{-# DEPRECATED lcarsCertificateAuthorities "Use generic-lens or generic-optics with 'certificateAuthorities' instead." #-}

-- | When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarsNextToken :: Lens.Lens' ListCertificateAuthoritiesResponse (Lude.Maybe Lude.Text)
lcarsNextToken = Lens.lens (nextToken :: ListCertificateAuthoritiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCertificateAuthoritiesResponse)
{-# DEPRECATED lcarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcarsResponseStatus :: Lens.Lens' ListCertificateAuthoritiesResponse Lude.Int
lcarsResponseStatus = Lens.lens (responseStatus :: ListCertificateAuthoritiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCertificateAuthoritiesResponse)
{-# DEPRECATED lcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
