{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.DomainValidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainValidation
  ( DomainValidation (..),

    -- * Smart constructor
    mkDomainValidation,

    -- * Lenses
    dvValidationEmails,
    dvValidationMethod,
    dvResourceRecord,
    dvValidationStatus,
    dvValidationDomain,
    dvDomainName,
  )
where

import Network.AWS.CertificateManager.Types.DomainStatus
import Network.AWS.CertificateManager.Types.ResourceRecord
import Network.AWS.CertificateManager.Types.ValidationMethod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the validation of each domain name in the certificate.
--
-- /See:/ 'mkDomainValidation' smart constructor.
data DomainValidation = DomainValidation'
  { validationEmails ::
      Lude.Maybe [Lude.Text],
    validationMethod :: Lude.Maybe ValidationMethod,
    resourceRecord :: Lude.Maybe ResourceRecord,
    validationStatus :: Lude.Maybe DomainStatus,
    validationDomain :: Lude.Maybe Lude.Text,
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainValidation' with the minimum fields required to make a request.
--
-- * 'domainName' - A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ .
-- * 'resourceRecord' - Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> .
--
-- Note: The CNAME information that you need does not include the name of your domain. If you include  your domain name in the DNS database CNAME record, validation fails.  For example, if the name is "_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com", only "_a79865eb4cd1a6ab990a45779b4e0b96" must be used.
-- * 'validationDomain' - The domain name that ACM used to send domain validation emails.
-- * 'validationEmails' - A list of email addresses that ACM used to send domain validation emails.
-- * 'validationMethod' - Specifies the domain validation method.
-- * 'validationStatus' - The validation status of the domain name. This can be one of the following values:
--
--
--     * @PENDING_VALIDATION@
--
--
--     * SUCCESS
--
--
--     * FAILED
mkDomainValidation ::
  -- | 'domainName'
  Lude.Text ->
  DomainValidation
mkDomainValidation pDomainName_ =
  DomainValidation'
    { validationEmails = Lude.Nothing,
      validationMethod = Lude.Nothing,
      resourceRecord = Lude.Nothing,
      validationStatus = Lude.Nothing,
      validationDomain = Lude.Nothing,
      domainName = pDomainName_
    }

-- | A list of email addresses that ACM used to send domain validation emails.
--
-- /Note:/ Consider using 'validationEmails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValidationEmails :: Lens.Lens' DomainValidation (Lude.Maybe [Lude.Text])
dvValidationEmails = Lens.lens (validationEmails :: DomainValidation -> Lude.Maybe [Lude.Text]) (\s a -> s {validationEmails = a} :: DomainValidation)
{-# DEPRECATED dvValidationEmails "Use generic-lens or generic-optics with 'validationEmails' instead." #-}

-- | Specifies the domain validation method.
--
-- /Note:/ Consider using 'validationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValidationMethod :: Lens.Lens' DomainValidation (Lude.Maybe ValidationMethod)
dvValidationMethod = Lens.lens (validationMethod :: DomainValidation -> Lude.Maybe ValidationMethod) (\s a -> s {validationMethod = a} :: DomainValidation)
{-# DEPRECATED dvValidationMethod "Use generic-lens or generic-optics with 'validationMethod' instead." #-}

-- | Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> .
--
-- Note: The CNAME information that you need does not include the name of your domain. If you include  your domain name in the DNS database CNAME record, validation fails.  For example, if the name is "_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com", only "_a79865eb4cd1a6ab990a45779b4e0b96" must be used.
--
-- /Note:/ Consider using 'resourceRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvResourceRecord :: Lens.Lens' DomainValidation (Lude.Maybe ResourceRecord)
dvResourceRecord = Lens.lens (resourceRecord :: DomainValidation -> Lude.Maybe ResourceRecord) (\s a -> s {resourceRecord = a} :: DomainValidation)
{-# DEPRECATED dvResourceRecord "Use generic-lens or generic-optics with 'resourceRecord' instead." #-}

-- | The validation status of the domain name. This can be one of the following values:
--
--
--     * @PENDING_VALIDATION@
--
--
--     * SUCCESS
--
--
--     * FAILED
--
--
--
-- /Note:/ Consider using 'validationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValidationStatus :: Lens.Lens' DomainValidation (Lude.Maybe DomainStatus)
dvValidationStatus = Lens.lens (validationStatus :: DomainValidation -> Lude.Maybe DomainStatus) (\s a -> s {validationStatus = a} :: DomainValidation)
{-# DEPRECATED dvValidationStatus "Use generic-lens or generic-optics with 'validationStatus' instead." #-}

-- | The domain name that ACM used to send domain validation emails.
--
-- /Note:/ Consider using 'validationDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValidationDomain :: Lens.Lens' DomainValidation (Lude.Maybe Lude.Text)
dvValidationDomain = Lens.lens (validationDomain :: DomainValidation -> Lude.Maybe Lude.Text) (\s a -> s {validationDomain = a} :: DomainValidation)
{-# DEPRECATED dvValidationDomain "Use generic-lens or generic-optics with 'validationDomain' instead." #-}

-- | A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvDomainName :: Lens.Lens' DomainValidation Lude.Text
dvDomainName = Lens.lens (domainName :: DomainValidation -> Lude.Text) (\s a -> s {domainName = a} :: DomainValidation)
{-# DEPRECATED dvDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.FromJSON DomainValidation where
  parseJSON =
    Lude.withObject
      "DomainValidation"
      ( \x ->
          DomainValidation'
            Lude.<$> (x Lude..:? "ValidationEmails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ValidationMethod")
            Lude.<*> (x Lude..:? "ResourceRecord")
            Lude.<*> (x Lude..:? "ValidationStatus")
            Lude.<*> (x Lude..:? "ValidationDomain")
            Lude.<*> (x Lude..: "DomainName")
      )
