{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.TagCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to your private CA. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a key and an optional value. You specify the private CA on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair. You can apply a tag to just one private CA if you want to identify a specific characteristic of that CA, or you can apply the same tag to multiple private CAs if you want to filter for a common relationship among those CAs. To remove one or more tags, use the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UntagCertificateAuthority.html UntagCertificateAuthority> action. Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListTags.html ListTags> action to see what tags are associated with your CA.
module Network.AWS.CertificateManagerPCA.TagCertificateAuthority
  ( -- * Creating a request
    TagCertificateAuthority (..),
    mkTagCertificateAuthority,

    -- ** Request lenses
    tcaCertificateAuthorityARN,
    tcaTags,

    -- * Destructuring the response
    TagCertificateAuthorityResponse (..),
    mkTagCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagCertificateAuthority' smart constructor.
data TagCertificateAuthority = TagCertificateAuthority'
  { certificateAuthorityARN ::
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

-- | Creates a value of 'TagCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
-- * 'tags' - List of tags to be associated with the CA.
mkTagCertificateAuthority ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  -- | 'tags'
  Lude.NonEmpty Tag ->
  TagCertificateAuthority
mkTagCertificateAuthority pCertificateAuthorityARN_ pTags_ =
  TagCertificateAuthority'
    { certificateAuthorityARN =
        pCertificateAuthorityARN_,
      tags = pTags_
    }

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaCertificateAuthorityARN :: Lens.Lens' TagCertificateAuthority Lude.Text
tcaCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: TagCertificateAuthority -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: TagCertificateAuthority)
{-# DEPRECATED tcaCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | List of tags to be associated with the CA.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaTags :: Lens.Lens' TagCertificateAuthority (Lude.NonEmpty Tag)
tcaTags = Lens.lens (tags :: TagCertificateAuthority -> Lude.NonEmpty Tag) (\s a -> s {tags = a} :: TagCertificateAuthority)
{-# DEPRECATED tcaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagCertificateAuthority where
  type Rs TagCertificateAuthority = TagCertificateAuthorityResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull TagCertificateAuthorityResponse'

instance Lude.ToHeaders TagCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.TagCertificateAuthority" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagCertificateAuthority where
  toJSON TagCertificateAuthority' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagCertificateAuthority where
  toPath = Lude.const "/"

instance Lude.ToQuery TagCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagCertificateAuthorityResponse' smart constructor.
data TagCertificateAuthorityResponse = TagCertificateAuthorityResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagCertificateAuthorityResponse' with the minimum fields required to make a request.
mkTagCertificateAuthorityResponse ::
  TagCertificateAuthorityResponse
mkTagCertificateAuthorityResponse =
  TagCertificateAuthorityResponse'
