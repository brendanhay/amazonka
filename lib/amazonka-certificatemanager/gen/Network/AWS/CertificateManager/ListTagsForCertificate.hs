{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ListTagsForCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been applied to the ACM certificate. Use the certificate's Amazon Resource Name (ARN) to specify the certificate. To add a tag to an ACM certificate, use the 'AddTagsToCertificate' action. To delete a tag, use the 'RemoveTagsFromCertificate' action.
module Network.AWS.CertificateManager.ListTagsForCertificate
  ( -- * Creating a request
    ListTagsForCertificate (..),
    mkListTagsForCertificate,

    -- ** Request lenses
    ltfcCertificateARN,

    -- * Destructuring the response
    ListTagsForCertificateResponse (..),
    mkListTagsForCertificateResponse,

    -- ** Response lenses
    ltfcrsTags,
    ltfcrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForCertificate' smart constructor.
newtype ListTagsForCertificate = ListTagsForCertificate'
  { -- | String that contains the ARN of the ACM certificate for which you want to list the tags. This must have the following form:
    --
    -- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    certificateARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains the ARN of the ACM certificate for which you want to list the tags. This must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkListTagsForCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  ListTagsForCertificate
mkListTagsForCertificate pCertificateARN_ =
  ListTagsForCertificate' {certificateARN = pCertificateARN_}

-- | String that contains the ARN of the ACM certificate for which you want to list the tags. This must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfcCertificateARN :: Lens.Lens' ListTagsForCertificate Lude.Text
ltfcCertificateARN = Lens.lens (certificateARN :: ListTagsForCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: ListTagsForCertificate)
{-# DEPRECATED ltfcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.AWSRequest ListTagsForCertificate where
  type Rs ListTagsForCertificate = ListTagsForCertificateResponse
  request = Req.postJSON certificateManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForCertificateResponse'
            Lude.<$> (x Lude..?> "Tags") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.ListTagsForCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForCertificate where
  toJSON ListTagsForCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateArn" Lude..= certificateARN)]
      )

instance Lude.ToPath ListTagsForCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForCertificateResponse' smart constructor.
data ListTagsForCertificateResponse = ListTagsForCertificateResponse'
  { -- | The key-value pairs that define the applied tags.
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForCertificateResponse' with the minimum fields required to make a request.
--
-- * 'tags' - The key-value pairs that define the applied tags.
-- * 'responseStatus' - The response status code.
mkListTagsForCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForCertificateResponse
mkListTagsForCertificateResponse pResponseStatus_ =
  ListTagsForCertificateResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The key-value pairs that define the applied tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfcrsTags :: Lens.Lens' ListTagsForCertificateResponse (Lude.Maybe (Lude.NonEmpty Tag))
ltfcrsTags = Lens.lens (tags :: ListTagsForCertificateResponse -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: ListTagsForCertificateResponse)
{-# DEPRECATED ltfcrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfcrsResponseStatus :: Lens.Lens' ListTagsForCertificateResponse Lude.Int
ltfcrsResponseStatus = Lens.lens (responseStatus :: ListTagsForCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForCertificateResponse)
{-# DEPRECATED ltfcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
