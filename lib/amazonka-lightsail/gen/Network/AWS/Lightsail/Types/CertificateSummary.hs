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
    cCertificateDetail,
    cCertificateName,
    cCertificateARN,
    cDomainName,
    cTags,
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
  { certificateDetail ::
      Lude.Maybe Certificate,
    certificateName :: Lude.Maybe Lude.Text,
    certificateARN :: Lude.Maybe Lude.Text,
    domainName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateSummary' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the certificate.
-- * 'certificateDetail' - An object that describes a certificate in detail.
-- * 'certificateName' - The name of the certificate.
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
cCertificateDetail :: Lens.Lens' CertificateSummary (Lude.Maybe Certificate)
cCertificateDetail = Lens.lens (certificateDetail :: CertificateSummary -> Lude.Maybe Certificate) (\s a -> s {certificateDetail = a} :: CertificateSummary)
{-# DEPRECATED cCertificateDetail "Use generic-lens or generic-optics with 'certificateDetail' instead." #-}

-- | The name of the certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateName :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
cCertificateName = Lens.lens (certificateName :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: CertificateSummary)
{-# DEPRECATED cCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateARN :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
cCertificateARN = Lens.lens (certificateARN :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CertificateSummary)
{-# DEPRECATED cCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The domain name of the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDomainName :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
cDomainName = Lens.lens (domainName :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: CertificateSummary)
{-# DEPRECATED cDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CertificateSummary (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: CertificateSummary -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CertificateSummary)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
