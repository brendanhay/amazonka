{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.DomainValidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.DomainValidation
  ( DomainValidation (..)
  -- * Smart constructor
  , mkDomainValidation
  -- * Lenses
  , dvDomainName
  , dvResourceRecord
  , dvValidationDomain
  , dvValidationEmails
  , dvValidationMethod
  , dvValidationStatus
  ) where

import qualified Network.AWS.CertificateManager.Types.DomainNameString as Types
import qualified Network.AWS.CertificateManager.Types.DomainStatus as Types
import qualified Network.AWS.CertificateManager.Types.ResourceRecord as Types
import qualified Network.AWS.CertificateManager.Types.ValidationMethod as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the validation of each domain name in the certificate.
--
-- /See:/ 'mkDomainValidation' smart constructor.
data DomainValidation = DomainValidation'
  { domainName :: Types.DomainNameString
    -- ^ A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ . 
  , resourceRecord :: Core.Maybe Types.ResourceRecord
    -- ^ Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> .
--
-- Note: The CNAME information that you need does not include the name of your domain. If you include  your domain name in the DNS database CNAME record, validation fails.  For example, if the name is "_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com", only "_a79865eb4cd1a6ab990a45779b4e0b96" must be used.
  , validationDomain :: Core.Maybe Types.DomainNameString
    -- ^ The domain name that ACM used to send domain validation emails.
  , validationEmails :: Core.Maybe [Core.Text]
    -- ^ A list of email addresses that ACM used to send domain validation emails.
  , validationMethod :: Core.Maybe Types.ValidationMethod
    -- ^ Specifies the domain validation method.
  , validationStatus :: Core.Maybe Types.DomainStatus
    -- ^ The validation status of the domain name. This can be one of the following values:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainValidation' value with any optional fields omitted.
mkDomainValidation
    :: Types.DomainNameString -- ^ 'domainName'
    -> DomainValidation
mkDomainValidation domainName
  = DomainValidation'{domainName, resourceRecord = Core.Nothing,
                      validationDomain = Core.Nothing, validationEmails = Core.Nothing,
                      validationMethod = Core.Nothing, validationStatus = Core.Nothing}

-- | A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ . 
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvDomainName :: Lens.Lens' DomainValidation Types.DomainNameString
dvDomainName = Lens.field @"domainName"
{-# INLINEABLE dvDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> .
--
-- Note: The CNAME information that you need does not include the name of your domain. If you include  your domain name in the DNS database CNAME record, validation fails.  For example, if the name is "_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com", only "_a79865eb4cd1a6ab990a45779b4e0b96" must be used.
--
-- /Note:/ Consider using 'resourceRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvResourceRecord :: Lens.Lens' DomainValidation (Core.Maybe Types.ResourceRecord)
dvResourceRecord = Lens.field @"resourceRecord"
{-# INLINEABLE dvResourceRecord #-}
{-# DEPRECATED resourceRecord "Use generic-lens or generic-optics with 'resourceRecord' instead"  #-}

-- | The domain name that ACM used to send domain validation emails.
--
-- /Note:/ Consider using 'validationDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValidationDomain :: Lens.Lens' DomainValidation (Core.Maybe Types.DomainNameString)
dvValidationDomain = Lens.field @"validationDomain"
{-# INLINEABLE dvValidationDomain #-}
{-# DEPRECATED validationDomain "Use generic-lens or generic-optics with 'validationDomain' instead"  #-}

-- | A list of email addresses that ACM used to send domain validation emails.
--
-- /Note:/ Consider using 'validationEmails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValidationEmails :: Lens.Lens' DomainValidation (Core.Maybe [Core.Text])
dvValidationEmails = Lens.field @"validationEmails"
{-# INLINEABLE dvValidationEmails #-}
{-# DEPRECATED validationEmails "Use generic-lens or generic-optics with 'validationEmails' instead"  #-}

-- | Specifies the domain validation method.
--
-- /Note:/ Consider using 'validationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValidationMethod :: Lens.Lens' DomainValidation (Core.Maybe Types.ValidationMethod)
dvValidationMethod = Lens.field @"validationMethod"
{-# INLINEABLE dvValidationMethod #-}
{-# DEPRECATED validationMethod "Use generic-lens or generic-optics with 'validationMethod' instead"  #-}

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
dvValidationStatus :: Lens.Lens' DomainValidation (Core.Maybe Types.DomainStatus)
dvValidationStatus = Lens.field @"validationStatus"
{-# INLINEABLE dvValidationStatus #-}
{-# DEPRECATED validationStatus "Use generic-lens or generic-optics with 'validationStatus' instead"  #-}

instance Core.FromJSON DomainValidation where
        parseJSON
          = Core.withObject "DomainValidation" Core.$
              \ x ->
                DomainValidation' Core.<$>
                  (x Core..: "DomainName") Core.<*> x Core..:? "ResourceRecord"
                    Core.<*> x Core..:? "ValidationDomain"
                    Core.<*> x Core..:? "ValidationEmails"
                    Core.<*> x Core..:? "ValidationMethod"
                    Core.<*> x Core..:? "ValidationStatus"
