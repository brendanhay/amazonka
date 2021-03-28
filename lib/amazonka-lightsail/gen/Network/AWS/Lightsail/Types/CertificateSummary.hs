{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.CertificateSummary
  ( CertificateSummary (..)
  -- * Smart constructor
  , mkCertificateSummary
  -- * Lenses
  , cCertificateArn
  , cCertificateDetail
  , cCertificateName
  , cDomainName
  , cTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Certificate as Types
import qualified Network.AWS.Lightsail.Types.CertificateArn as Types
import qualified Network.AWS.Lightsail.Types.CertificateName as Types
import qualified Network.AWS.Lightsail.Types.DomainName as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon Lightsail SSL/TLS certificate.
--
-- /See:/ 'mkCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The Amazon Resource Name (ARN) of the certificate.
  , certificateDetail :: Core.Maybe Types.Certificate
    -- ^ An object that describes a certificate in detail.
  , certificateName :: Core.Maybe Types.CertificateName
    -- ^ The name of the certificate.
  , domainName :: Core.Maybe Types.DomainName
    -- ^ The domain name of the certificate.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CertificateSummary' value with any optional fields omitted.
mkCertificateSummary
    :: CertificateSummary
mkCertificateSummary
  = CertificateSummary'{certificateArn = Core.Nothing,
                        certificateDetail = Core.Nothing, certificateName = Core.Nothing,
                        domainName = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' CertificateSummary (Core.Maybe Types.CertificateArn)
cCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | An object that describes a certificate in detail.
--
-- /Note:/ Consider using 'certificateDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateDetail :: Lens.Lens' CertificateSummary (Core.Maybe Types.Certificate)
cCertificateDetail = Lens.field @"certificateDetail"
{-# INLINEABLE cCertificateDetail #-}
{-# DEPRECATED certificateDetail "Use generic-lens or generic-optics with 'certificateDetail' instead"  #-}

-- | The name of the certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateName :: Lens.Lens' CertificateSummary (Core.Maybe Types.CertificateName)
cCertificateName = Lens.field @"certificateName"
{-# INLINEABLE cCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | The domain name of the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDomainName :: Lens.Lens' CertificateSummary (Core.Maybe Types.DomainName)
cDomainName = Lens.field @"domainName"
{-# INLINEABLE cDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CertificateSummary (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON CertificateSummary where
        parseJSON
          = Core.withObject "CertificateSummary" Core.$
              \ x ->
                CertificateSummary' Core.<$>
                  (x Core..:? "certificateArn") Core.<*>
                    x Core..:? "certificateDetail"
                    Core.<*> x Core..:? "certificateName"
                    Core.<*> x Core..:? "domainName"
                    Core.<*> x Core..:? "tags"
