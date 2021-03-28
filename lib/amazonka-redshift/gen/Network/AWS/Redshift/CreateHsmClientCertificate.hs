{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateHsmClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM client certificate that an Amazon Redshift cluster will use to connect to the client's HSM in order to store and retrieve the keys used to encrypt the cluster databases.
--
-- The command returns a public key, which you must store in the HSM. In addition to creating the HSM certificate, you must create an Amazon Redshift HSM configuration that provides a cluster the information needed to store and use encryption keys in the HSM. For more information, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules> in the Amazon Redshift Cluster Management Guide.
module Network.AWS.Redshift.CreateHsmClientCertificate
    (
    -- * Creating a request
      CreateHsmClientCertificate (..)
    , mkCreateHsmClientCertificate
    -- ** Request lenses
    , chccHsmClientCertificateIdentifier
    , chccTags

    -- * Destructuring the response
    , CreateHsmClientCertificateResponse (..)
    , mkCreateHsmClientCertificateResponse
    -- ** Response lenses
    , chccrrsHsmClientCertificate
    , chccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateHsmClientCertificate' smart constructor.
data CreateHsmClientCertificate = CreateHsmClientCertificate'
  { hsmClientCertificateIdentifier :: Core.Text
    -- ^ The identifier to be assigned to the new HSM client certificate that the cluster will use to connect to the HSM to use the database encryption keys.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsmClientCertificate' value with any optional fields omitted.
mkCreateHsmClientCertificate
    :: Core.Text -- ^ 'hsmClientCertificateIdentifier'
    -> CreateHsmClientCertificate
mkCreateHsmClientCertificate hsmClientCertificateIdentifier
  = CreateHsmClientCertificate'{hsmClientCertificateIdentifier,
                                tags = Core.Nothing}

-- | The identifier to be assigned to the new HSM client certificate that the cluster will use to connect to the HSM to use the database encryption keys.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccHsmClientCertificateIdentifier :: Lens.Lens' CreateHsmClientCertificate Core.Text
chccHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE chccHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccTags :: Lens.Lens' CreateHsmClientCertificate (Core.Maybe [Types.Tag])
chccTags = Lens.field @"tags"
{-# INLINEABLE chccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateHsmClientCertificate where
        toQuery CreateHsmClientCertificate{..}
          = Core.toQueryPair "Action"
              ("CreateHsmClientCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "HsmClientCertificateIdentifier"
                hsmClientCertificateIdentifier
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateHsmClientCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateHsmClientCertificate where
        type Rs CreateHsmClientCertificate =
             CreateHsmClientCertificateResponse
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
          = Response.receiveXMLWrapper "CreateHsmClientCertificateResult"
              (\ s h x ->
                 CreateHsmClientCertificateResponse' Core.<$>
                   (x Core..@? "HsmClientCertificate") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateHsmClientCertificateResponse' smart constructor.
data CreateHsmClientCertificateResponse = CreateHsmClientCertificateResponse'
  { hsmClientCertificate :: Core.Maybe Types.HsmClientCertificate
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHsmClientCertificateResponse' value with any optional fields omitted.
mkCreateHsmClientCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateHsmClientCertificateResponse
mkCreateHsmClientCertificateResponse responseStatus
  = CreateHsmClientCertificateResponse'{hsmClientCertificate =
                                          Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hsmClientCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccrrsHsmClientCertificate :: Lens.Lens' CreateHsmClientCertificateResponse (Core.Maybe Types.HsmClientCertificate)
chccrrsHsmClientCertificate = Lens.field @"hsmClientCertificate"
{-# INLINEABLE chccrrsHsmClientCertificate #-}
{-# DEPRECATED hsmClientCertificate "Use generic-lens or generic-optics with 'hsmClientCertificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccrrsResponseStatus :: Lens.Lens' CreateHsmClientCertificateResponse Core.Int
chccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE chccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
