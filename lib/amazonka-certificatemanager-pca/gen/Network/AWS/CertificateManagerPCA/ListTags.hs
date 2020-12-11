{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags, if any, that are associated with your private CA or one that has been shared with you. Tags are labels that you can use to identify and organize your CAs. Each tag consists of a key and an optional value. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_TagCertificateAuthority.html TagCertificateAuthority> action to add one or more tags to your CA. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UntagCertificateAuthority.html UntagCertificateAuthority> action to remove tags.
--
-- This operation returns paginated results.
module Network.AWS.CertificateManagerPCA.ListTags
  ( -- * Creating a request
    ListTags (..),
    mkListTags,

    -- ** Request lenses
    ltNextToken,
    ltMaxResults,
    ltCertificateAuthorityARN,

    -- * Destructuring the response
    ListTagsResponse (..),
    mkListTagsResponse,

    -- ** Response lenses
    ltrsNextToken,
    ltrsTags,
    ltrsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
-- * 'maxResults' - Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
-- * 'nextToken' - Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
mkListTags ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  ListTags
mkListTags pCertificateAuthorityARN_ =
  ListTags'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      certificateAuthorityARN = pCertificateAuthorityARN_
    }

-- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTags (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTags -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTags)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTags (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListTags -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTags)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCertificateAuthorityARN :: Lens.Lens' ListTags Lude.Text
ltCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: ListTags -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: ListTags)
{-# DEPRECATED ltCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Page.AWSPager ListTags where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTags where
  type Rs ListTags = ListTagsResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Tags")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.ListTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTags where
  toJSON ListTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath ListTags where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags associated with your private CA.
mkListTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsResponse
mkListTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTagsResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The tags associated with your private CA.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTags :: Lens.Lens' ListTagsResponse (Lude.Maybe (Lude.NonEmpty Tag))
ltrsTags = Lens.lens (tags :: ListTagsResponse -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: ListTagsResponse)
{-# DEPRECATED ltrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTagsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
