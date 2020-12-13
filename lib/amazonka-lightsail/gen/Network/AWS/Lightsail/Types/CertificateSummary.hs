{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CertificateSummary
  ( CertificateSummary (..),

    -- * Smart constructor
    mkCertificateSummary,

    -- * Lenses
    csfCertificateDetail,
    csfCertificateName,
    csfCertificateARN,
    csfDomainName,
    csfTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Certificate
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon Lightsail SSL/TLS certificate.
--
-- /See:/ 'mkCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { -- | An object that describes a certificate in detail.
    certificateDetail :: Lude.Maybe Certificate,
    -- | The name of the certificate.
    certificateName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The domain name of the certificate.
    domainName :: Lude.Maybe Lude.Text,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateSummary' with the minimum fields required to make a request.
--
-- * 'certificateDetail' - An object that describes a certificate in detail.
-- * 'certificateName' - The name of the certificate.
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the certificate.
-- * 'domainName' - The domain name of the certificate.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkCertificateSummary ::
  CertificateSummary
mkCertificateSummary =
  CertificateSummary'
    { certificateDetail = Lude.Nothing,
      certificateName = Lude.Nothing,
      certificateARN = Lude.Nothing,
      domainName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | An object that describes a certificate in detail.
--
-- /Note:/ Consider using 'certificateDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfCertificateDetail :: Lens.Lens' CertificateSummary (Lude.Maybe Certificate)
csfCertificateDetail = Lens.lens (certificateDetail :: CertificateSummary -> Lude.Maybe Certificate) (\s a -> s {certificateDetail = a} :: CertificateSummary)
{-# DEPRECATED csfCertificateDetail "Use generic-lens or generic-optics with 'certificateDetail' instead." #-}

-- | The name of the certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfCertificateName :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
csfCertificateName = Lens.lens (certificateName :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: CertificateSummary)
{-# DEPRECATED csfCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfCertificateARN :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
csfCertificateARN = Lens.lens (certificateARN :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CertificateSummary)
{-# DEPRECATED csfCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The domain name of the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDomainName :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
csfDomainName = Lens.lens (domainName :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: CertificateSummary)
{-# DEPRECATED csfDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTags :: Lens.Lens' CertificateSummary (Lude.Maybe [Tag])
csfTags = Lens.lens (tags :: CertificateSummary -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CertificateSummary)
{-# DEPRECATED csfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON CertificateSummary where
  parseJSON =
    Lude.withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            Lude.<$> (x Lude..:? "certificateDetail")
            Lude.<*> (x Lude..:? "certificateName")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "domainName")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
