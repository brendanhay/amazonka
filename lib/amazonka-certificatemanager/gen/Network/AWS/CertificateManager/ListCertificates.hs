{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ListCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of certificate ARNs and domain names. You can request that only certificates that match a specific status be listed. You can also filter by specific attributes of the certificate. Default filtering returns only @RSA_2048@ certificates. For more information, see 'Filters' .
--
-- This operation returns paginated results.
module Network.AWS.CertificateManager.ListCertificates
  ( -- * Creating a request
    ListCertificates (..),
    mkListCertificates,

    -- ** Request lenses
    lcCertificateStatuses,
    lcNextToken,
    lcIncludes,
    lcMaxItems,

    -- * Destructuring the response
    ListCertificatesResponse (..),
    mkListCertificatesResponse,

    -- ** Response lenses
    lcrsCertificateSummaryList,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { -- | Filter the certificate list by status value.
    certificateStatuses :: Lude.Maybe [CertificateStatus],
    -- | Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of @NextToken@ from the response you just received.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filter the certificate list. For more information, see the 'Filters' structure.
    includes :: Lude.Maybe Filters,
    -- | Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCertificates' with the minimum fields required to make a request.
--
-- * 'certificateStatuses' - Filter the certificate list by status value.
-- * 'nextToken' - Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of @NextToken@ from the response you just received.
-- * 'includes' - Filter the certificate list. For more information, see the 'Filters' structure.
-- * 'maxItems' - Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
mkListCertificates ::
  ListCertificates
mkListCertificates =
  ListCertificates'
    { certificateStatuses = Lude.Nothing,
      nextToken = Lude.Nothing,
      includes = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Filter the certificate list by status value.
--
-- /Note:/ Consider using 'certificateStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCertificateStatuses :: Lens.Lens' ListCertificates (Lude.Maybe [CertificateStatus])
lcCertificateStatuses = Lens.lens (certificateStatuses :: ListCertificates -> Lude.Maybe [CertificateStatus]) (\s a -> s {certificateStatuses = a} :: ListCertificates)
{-# DEPRECATED lcCertificateStatuses "Use generic-lens or generic-optics with 'certificateStatuses' instead." #-}

-- | Use this parameter only when paginating results and only in a subsequent request after you receive a response with truncated results. Set it to the value of @NextToken@ from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCertificates (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListCertificates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCertificates)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter the certificate list. For more information, see the 'Filters' structure.
--
-- /Note:/ Consider using 'includes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcIncludes :: Lens.Lens' ListCertificates (Lude.Maybe Filters)
lcIncludes = Lens.lens (includes :: ListCertificates -> Lude.Maybe Filters) (\s a -> s {includes = a} :: ListCertificates)
{-# DEPRECATED lcIncludes "Use generic-lens or generic-optics with 'includes' instead." #-}

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxItems :: Lens.Lens' ListCertificates (Lude.Maybe Lude.Natural)
lcMaxItems = Lens.lens (maxItems :: ListCertificates -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListCertificates)
{-# DEPRECATED lcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListCertificates where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsCertificateSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request = Req.postJSON certificateManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Lude.<$> (x Lude..?> "CertificateSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCertificates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.ListCertificates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCertificates where
  toJSON ListCertificates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CertificateStatuses" Lude..=) Lude.<$> certificateStatuses,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Includes" Lude..=) Lude.<$> includes,
            ("MaxItems" Lude..=) Lude.<$> maxItems
          ]
      )

instance Lude.ToPath ListCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCertificates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { -- | A list of ACM certificates.
    certificateSummaryList :: Lude.Maybe [CertificateSummary],
    -- | When the list is truncated, this value is present and contains the value to use for the @NextToken@ parameter in a subsequent pagination request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificateSummaryList' - A list of ACM certificates.
-- * 'nextToken' - When the list is truncated, this value is present and contains the value to use for the @NextToken@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
mkListCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCertificatesResponse
mkListCertificatesResponse pResponseStatus_ =
  ListCertificatesResponse'
    { certificateSummaryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of ACM certificates.
--
-- /Note:/ Consider using 'certificateSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsCertificateSummaryList :: Lens.Lens' ListCertificatesResponse (Lude.Maybe [CertificateSummary])
lcrsCertificateSummaryList = Lens.lens (certificateSummaryList :: ListCertificatesResponse -> Lude.Maybe [CertificateSummary]) (\s a -> s {certificateSummaryList = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsCertificateSummaryList "Use generic-lens or generic-optics with 'certificateSummaryList' instead." #-}

-- | When the list is truncated, this value is present and contains the value to use for the @NextToken@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListCertificatesResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListCertificatesResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
