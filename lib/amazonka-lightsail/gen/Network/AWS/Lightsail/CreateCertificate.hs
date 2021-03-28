{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSL/TLS certificate for a Amazon Lightsail content delivery network (CDN) distribution.
--
-- After the certificate is created, use the @AttachCertificateToDistribution@ action to attach the certificate to your distribution.
-- /Important:/ Only certificates created in the @us-east-1@ AWS Region can be attached to Lightsail distributions. Lightsail distributions are global resources that can reference an origin in any AWS Region, and distribute its content globally. However, all distributions are located in the @us-east-1@ Region.
module Network.AWS.Lightsail.CreateCertificate
    (
    -- * Creating a request
      CreateCertificate (..)
    , mkCreateCertificate
    -- ** Request lenses
    , ccCertificateName
    , ccDomainName
    , ccSubjectAlternativeNames
    , ccTags

    -- * Destructuring the response
    , CreateCertificateResponse (..)
    , mkCreateCertificateResponse
    -- ** Response lenses
    , ccrrsCertificate
    , ccrrsOperations
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCertificate' smart constructor.
data CreateCertificate = CreateCertificate'
  { certificateName :: Types.CertificateName
    -- ^ The name for the certificate.
  , domainName :: Types.DomainName
    -- ^ The domain name (e.g., @example.com@ ) for the certificate.
  , subjectAlternativeNames :: Core.Maybe [Types.DomainName]
    -- ^ An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
--
-- You can specify a maximum of nine alternate domains (in addition to the primary domain name).
-- Wildcard domain entries (e.g., @*.example.com@ ) are not supported.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the certificate during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCertificate' value with any optional fields omitted.
mkCreateCertificate
    :: Types.CertificateName -- ^ 'certificateName'
    -> Types.DomainName -- ^ 'domainName'
    -> CreateCertificate
mkCreateCertificate certificateName domainName
  = CreateCertificate'{certificateName, domainName,
                       subjectAlternativeNames = Core.Nothing, tags = Core.Nothing}

-- | The name for the certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCertificateName :: Lens.Lens' CreateCertificate Types.CertificateName
ccCertificateName = Lens.field @"certificateName"
{-# INLINEABLE ccCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | The domain name (e.g., @example.com@ ) for the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDomainName :: Lens.Lens' CreateCertificate Types.DomainName
ccDomainName = Lens.field @"domainName"
{-# INLINEABLE ccDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
--
-- You can specify a maximum of nine alternate domains (in addition to the primary domain name).
-- Wildcard domain entries (e.g., @*.example.com@ ) are not supported.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSubjectAlternativeNames :: Lens.Lens' CreateCertificate (Core.Maybe [Types.DomainName])
ccSubjectAlternativeNames = Lens.field @"subjectAlternativeNames"
{-# INLINEABLE ccSubjectAlternativeNames #-}
{-# DEPRECATED subjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead"  #-}

-- | The tag keys and optional values to add to the certificate during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateCertificate (Core.Maybe [Types.Tag])
ccTags = Lens.field @"tags"
{-# INLINEABLE ccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCertificate where
        toHeaders CreateCertificate{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCertificate where
        toJSON CreateCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("certificateName" Core..= certificateName),
                  Core.Just ("domainName" Core..= domainName),
                  ("subjectAlternativeNames" Core..=) Core.<$>
                    subjectAlternativeNames,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCertificate where
        type Rs CreateCertificate = CreateCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCertificateResponse' Core.<$>
                   (x Core..:? "certificate") Core.<*> x Core..:? "operations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCertificateResponse' smart constructor.
data CreateCertificateResponse = CreateCertificateResponse'
  { certificate :: Core.Maybe Types.CertificateSummary
    -- ^ An object that describes the certificate created.
  , operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateCertificateResponse' value with any optional fields omitted.
mkCreateCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCertificateResponse
mkCreateCertificateResponse responseStatus
  = CreateCertificateResponse'{certificate = Core.Nothing,
                               operations = Core.Nothing, responseStatus}

-- | An object that describes the certificate created.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCertificate :: Lens.Lens' CreateCertificateResponse (Core.Maybe Types.CertificateSummary)
ccrrsCertificate = Lens.field @"certificate"
{-# INLINEABLE ccrrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsOperations :: Lens.Lens' CreateCertificateResponse (Core.Maybe [Types.Operation])
ccrrsOperations = Lens.field @"operations"
{-# INLINEABLE ccrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCertificateResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
