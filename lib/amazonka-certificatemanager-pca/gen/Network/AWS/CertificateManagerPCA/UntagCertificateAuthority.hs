{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more tags from your private CA. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this action, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value. To add tags to a private CA, use the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_TagCertificateAuthority.html TagCertificateAuthority> . Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListTags.html ListTags> action to see what tags are associated with your CA.
module Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
  ( -- * Creating a request
    UntagCertificateAuthority (..),
    mkUntagCertificateAuthority,

    -- ** Request lenses
    uCertificateAuthorityARN,
    uTags,

    -- * Destructuring the response
    UntagCertificateAuthorityResponse (..),
    mkUntagCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagCertificateAuthority' smart constructor.
data UntagCertificateAuthority = UntagCertificateAuthority'
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

-- | Creates a value of 'UntagCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
-- * 'tags' - List of tags to be removed from the CA.
mkUntagCertificateAuthority ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  -- | 'tags'
  Lude.NonEmpty Tag ->
  UntagCertificateAuthority
mkUntagCertificateAuthority pCertificateAuthorityARN_ pTags_ =
  UntagCertificateAuthority'
    { certificateAuthorityARN =
        pCertificateAuthorityARN_,
      tags = pTags_
    }

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCertificateAuthorityARN :: Lens.Lens' UntagCertificateAuthority Lude.Text
uCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: UntagCertificateAuthority -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: UntagCertificateAuthority)
{-# DEPRECATED uCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | List of tags to be removed from the CA.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' UntagCertificateAuthority (Lude.NonEmpty Tag)
uTags = Lens.lens (tags :: UntagCertificateAuthority -> Lude.NonEmpty Tag) (\s a -> s {tags = a} :: UntagCertificateAuthority)
{-# DEPRECATED uTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest UntagCertificateAuthority where
  type
    Rs UntagCertificateAuthority =
      UntagCertificateAuthorityResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull UntagCertificateAuthorityResponse'

instance Lude.ToHeaders UntagCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.UntagCertificateAuthority" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagCertificateAuthority where
  toJSON UntagCertificateAuthority' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath UntagCertificateAuthority where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagCertificateAuthorityResponse' smart constructor.
data UntagCertificateAuthorityResponse = UntagCertificateAuthorityResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagCertificateAuthorityResponse' with the minimum fields required to make a request.
mkUntagCertificateAuthorityResponse ::
  UntagCertificateAuthorityResponse
mkUntagCertificateAuthorityResponse =
  UntagCertificateAuthorityResponse'
