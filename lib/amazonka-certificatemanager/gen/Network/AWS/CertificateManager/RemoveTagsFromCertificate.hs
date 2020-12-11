{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.RemoveTagsFromCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more tags from an ACM certificate. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this function, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value.
--
-- To add tags to a certificate, use the 'AddTagsToCertificate' action. To view all of the tags that have been applied to a specific ACM certificate, use the 'ListTagsForCertificate' action.
module Network.AWS.CertificateManager.RemoveTagsFromCertificate
  ( -- * Creating a request
    RemoveTagsFromCertificate (..),
    mkRemoveTagsFromCertificate,

    -- ** Request lenses
    rtfcCertificateARN,
    rtfcTags,

    -- * Destructuring the response
    RemoveTagsFromCertificateResponse (..),
    mkRemoveTagsFromCertificateResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveTagsFromCertificate' smart constructor.
data RemoveTagsFromCertificate = RemoveTagsFromCertificate'
  { certificateARN ::
      Lude.Text,
    tags :: Lude.NonEmpty Tag
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains the ARN of the ACM Certificate with one or more tags that you want to remove. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'tags' - The key-value pair that defines the tag to remove.
mkRemoveTagsFromCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  -- | 'tags'
  Lude.NonEmpty Tag ->
  RemoveTagsFromCertificate
mkRemoveTagsFromCertificate pCertificateARN_ pTags_ =
  RemoveTagsFromCertificate'
    { certificateARN = pCertificateARN_,
      tags = pTags_
    }

-- | String that contains the ARN of the ACM Certificate with one or more tags that you want to remove. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcCertificateARN :: Lens.Lens' RemoveTagsFromCertificate Lude.Text
rtfcCertificateARN = Lens.lens (certificateARN :: RemoveTagsFromCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: RemoveTagsFromCertificate)
{-# DEPRECATED rtfcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The key-value pair that defines the tag to remove.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfcTags :: Lens.Lens' RemoveTagsFromCertificate (Lude.NonEmpty Tag)
rtfcTags = Lens.lens (tags :: RemoveTagsFromCertificate -> Lude.NonEmpty Tag) (\s a -> s {tags = a} :: RemoveTagsFromCertificate)
{-# DEPRECATED rtfcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest RemoveTagsFromCertificate where
  type
    Rs RemoveTagsFromCertificate =
      RemoveTagsFromCertificateResponse
  request = Req.postJSON certificateManagerService
  response = Res.receiveNull RemoveTagsFromCertificateResponse'

instance Lude.ToHeaders RemoveTagsFromCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CertificateManager.RemoveTagsFromCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTagsFromCertificate where
  toJSON RemoveTagsFromCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CertificateArn" Lude..= certificateARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath RemoveTagsFromCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveTagsFromCertificateResponse' smart constructor.
data RemoveTagsFromCertificateResponse = RemoveTagsFromCertificateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromCertificateResponse' with the minimum fields required to make a request.
mkRemoveTagsFromCertificateResponse ::
  RemoveTagsFromCertificateResponse
mkRemoveTagsFromCertificateResponse =
  RemoveTagsFromCertificateResponse'
