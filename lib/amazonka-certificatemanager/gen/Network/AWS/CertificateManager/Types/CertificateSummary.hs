{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.CertificateSummary
  ( CertificateSummary (..)
  -- * Smart constructor
  , mkCertificateSummary
  -- * Lenses
  , csCertificateArn
  , csDomainName
  ) where

import qualified Network.AWS.CertificateManager.Types.Arn as Types
import qualified Network.AWS.CertificateManager.Types.DomainName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure is returned in the response object of 'ListCertificates' action. 
--
-- /See:/ 'mkCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { certificateArn :: Core.Maybe Types.Arn
    -- ^ Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . 
  , domainName :: Core.Maybe Types.DomainName
    -- ^ Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CertificateSummary' value with any optional fields omitted.
mkCertificateSummary
    :: CertificateSummary
mkCertificateSummary
  = CertificateSummary'{certificateArn = Core.Nothing,
                        domainName = Core.Nothing}

-- | Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . 
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCertificateArn :: Lens.Lens' CertificateSummary (Core.Maybe Types.Arn)
csCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE csCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDomainName :: Lens.Lens' CertificateSummary (Core.Maybe Types.DomainName)
csDomainName = Lens.field @"domainName"
{-# INLINEABLE csDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.FromJSON CertificateSummary where
        parseJSON
          = Core.withObject "CertificateSummary" Core.$
              \ x ->
                CertificateSummary' Core.<$>
                  (x Core..:? "CertificateArn") Core.<*> x Core..:? "DomainName"
