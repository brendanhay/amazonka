{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.AddTagsToCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an ACM certificate. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a @key@ and an optional @value@ . You specify the certificate on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair.
--
-- You can apply a tag to just one certificate if you want to identify a specific characteristic of that certificate, or you can apply the same tag to multiple certificates if you want to filter for a common relationship among those certificates. Similarly, you can apply the same tag to multiple resources if you want to specify a relationship among those resources. For example, you can add the same tag to an ACM certificate and an Elastic Load Balancing load balancer to indicate that they are both used by the same website. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/tags.html Tagging ACM certificates> .
-- To remove one or more tags, use the 'RemoveTagsFromCertificate' action. To view all of the tags that have been applied to the certificate, use the 'ListTagsForCertificate' action.
module Network.AWS.CertificateManager.AddTagsToCertificate
  ( -- * Creating a request
    AddTagsToCertificate (..),
    mkAddTagsToCertificate,

    -- ** Request lenses
    attcCertificateARN,
    attcTags,

    -- * Destructuring the response
    AddTagsToCertificateResponse (..),
    mkAddTagsToCertificateResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddTagsToCertificate' smart constructor.
data AddTagsToCertificate = AddTagsToCertificate'
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

-- | Creates a value of 'AddTagsToCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - String that contains the ARN of the ACM certificate to which the tag is to be applied. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'tags' - The key-value pair that defines the tag. The tag value is optional.
mkAddTagsToCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  -- | 'tags'
  Lude.NonEmpty Tag ->
  AddTagsToCertificate
mkAddTagsToCertificate pCertificateARN_ pTags_ =
  AddTagsToCertificate'
    { certificateARN = pCertificateARN_,
      tags = pTags_
    }

-- | String that contains the ARN of the ACM certificate to which the tag is to be applied. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attcCertificateARN :: Lens.Lens' AddTagsToCertificate Lude.Text
attcCertificateARN = Lens.lens (certificateARN :: AddTagsToCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: AddTagsToCertificate)
{-# DEPRECATED attcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The key-value pair that defines the tag. The tag value is optional.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attcTags :: Lens.Lens' AddTagsToCertificate (Lude.NonEmpty Tag)
attcTags = Lens.lens (tags :: AddTagsToCertificate -> Lude.NonEmpty Tag) (\s a -> s {tags = a} :: AddTagsToCertificate)
{-# DEPRECATED attcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToCertificate where
  type Rs AddTagsToCertificate = AddTagsToCertificateResponse
  request = Req.postJSON certificateManagerService
  response = Res.receiveNull AddTagsToCertificateResponse'

instance Lude.ToHeaders AddTagsToCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.AddTagsToCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToCertificate where
  toJSON AddTagsToCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CertificateArn" Lude..= certificateARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTagsToCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsToCertificateResponse' smart constructor.
data AddTagsToCertificateResponse = AddTagsToCertificateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToCertificateResponse' with the minimum fields required to make a request.
mkAddTagsToCertificateResponse ::
  AddTagsToCertificateResponse
mkAddTagsToCertificateResponse = AddTagsToCertificateResponse'
