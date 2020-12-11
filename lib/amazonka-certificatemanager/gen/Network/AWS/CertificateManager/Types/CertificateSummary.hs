-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateSummary
  ( CertificateSummary (..),

    -- * Smart constructor
    mkCertificateSummary,

    -- * Lenses
    csCertificateARN,
    csDomainName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This structure is returned in the response object of 'ListCertificates' action.
--
-- /See:/ 'mkCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { certificateARN ::
      Lude.Maybe Lude.Text,
    domainName :: Lude.Maybe Lude.Text
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
-- * 'certificateARN' - Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'domainName' - Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
mkCertificateSummary ::
  CertificateSummary
mkCertificateSummary =
  CertificateSummary'
    { certificateARN = Lude.Nothing,
      domainName = Lude.Nothing
    }

-- | Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCertificateARN :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
csCertificateARN = Lens.lens (certificateARN :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CertificateSummary)
{-# DEPRECATED csCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDomainName :: Lens.Lens' CertificateSummary (Lude.Maybe Lude.Text)
csDomainName = Lens.lens (domainName :: CertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: CertificateSummary)
{-# DEPRECATED csDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.FromJSON CertificateSummary where
  parseJSON =
    Lude.withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            Lude.<$> (x Lude..:? "CertificateArn") Lude.<*> (x Lude..:? "DomainName")
      )
