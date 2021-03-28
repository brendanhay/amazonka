{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Override the system-default Secure Sockets Layer/Transport Layer Security (SSL/TLS) certificate for Amazon RDS for new DB instances temporarily, or remove the override.
--
-- By using this operation, you can specify an RDS-approved SSL/TLS certificate for new DB instances that is different from the default certificate provided by RDS. You can also use this operation to remove the override, so that new DB instances use the default certificate provided by RDS.
-- You might need to override the default certificate in the following situations:
--
--     * You already migrated your applications to support the latest certificate authority (CA) certificate, but the new CA certificate is not yet the RDS default CA certificate for the specified AWS Region.
--
--
--     * RDS has already moved to a new default CA certificate for the specified AWS Region, but you are still in the process of supporting the new CA certificate. In this case, you temporarily need additional time to finish your application changes.
--
--
-- For more information about rotating your SSL/TLS certificate for RDS DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon RDS User Guide/ .
-- For more information about rotating your SSL/TLS certificate for Aurora DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.ModifyCertificates
    (
    -- * Creating a request
      ModifyCertificates (..)
    , mkModifyCertificates
    -- ** Request lenses
    , mcCertificateIdentifier
    , mcRemoveCustomerOverride

    -- * Destructuring the response
    , ModifyCertificatesResponse (..)
    , mkModifyCertificatesResponse
    -- ** Response lenses
    , mcrrsCertificate
    , mcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyCertificates' smart constructor.
data ModifyCertificates = ModifyCertificates'
  { certificateIdentifier :: Core.Maybe Core.Text
    -- ^ The new default certificate identifier to override the current one with.
--
-- To determine the valid values, use the @describe-certificates@ AWS CLI command or the @DescribeCertificates@ API operation.
  , removeCustomerOverride :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to remove the override for the default certificate. If the override is removed, the default certificate is the system default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCertificates' value with any optional fields omitted.
mkModifyCertificates
    :: ModifyCertificates
mkModifyCertificates
  = ModifyCertificates'{certificateIdentifier = Core.Nothing,
                        removeCustomerOverride = Core.Nothing}

-- | The new default certificate identifier to override the current one with.
--
-- To determine the valid values, use the @describe-certificates@ AWS CLI command or the @DescribeCertificates@ API operation.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcCertificateIdentifier :: Lens.Lens' ModifyCertificates (Core.Maybe Core.Text)
mcCertificateIdentifier = Lens.field @"certificateIdentifier"
{-# INLINEABLE mcCertificateIdentifier #-}
{-# DEPRECATED certificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead"  #-}

-- | A value that indicates whether to remove the override for the default certificate. If the override is removed, the default certificate is the system default.
--
-- /Note:/ Consider using 'removeCustomerOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcRemoveCustomerOverride :: Lens.Lens' ModifyCertificates (Core.Maybe Core.Bool)
mcRemoveCustomerOverride = Lens.field @"removeCustomerOverride"
{-# INLINEABLE mcRemoveCustomerOverride #-}
{-# DEPRECATED removeCustomerOverride "Use generic-lens or generic-optics with 'removeCustomerOverride' instead"  #-}

instance Core.ToQuery ModifyCertificates where
        toQuery ModifyCertificates{..}
          = Core.toQueryPair "Action" ("ModifyCertificates" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CertificateIdentifier")
                certificateIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RemoveCustomerOverride")
                removeCustomerOverride

instance Core.ToHeaders ModifyCertificates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyCertificates where
        type Rs ModifyCertificates = ModifyCertificatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyCertificatesResult"
              (\ s h x ->
                 ModifyCertificatesResponse' Core.<$>
                   (x Core..@? "Certificate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyCertificatesResponse' smart constructor.
data ModifyCertificatesResponse = ModifyCertificatesResponse'
  { certificate :: Core.Maybe Types.Certificate
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyCertificatesResponse' value with any optional fields omitted.
mkModifyCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyCertificatesResponse
mkModifyCertificatesResponse responseStatus
  = ModifyCertificatesResponse'{certificate = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsCertificate :: Lens.Lens' ModifyCertificatesResponse (Core.Maybe Types.Certificate)
mcrrsCertificate = Lens.field @"certificate"
{-# INLINEABLE mcrrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsResponseStatus :: Lens.Lens' ModifyCertificatesResponse Core.Int
mcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
