{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteHsmClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified HSM client certificate.
module Network.AWS.Redshift.DeleteHsmClientCertificate
    (
    -- * Creating a request
      DeleteHsmClientCertificate (..)
    , mkDeleteHsmClientCertificate
    -- ** Request lenses
    , dHsmClientCertificateIdentifier

    -- * Destructuring the response
    , DeleteHsmClientCertificateResponse (..)
    , mkDeleteHsmClientCertificateResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteHsmClientCertificate' smart constructor.
newtype DeleteHsmClientCertificate = DeleteHsmClientCertificate'
  { hsmClientCertificateIdentifier :: Core.Text
    -- ^ The identifier of the HSM client certificate to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsmClientCertificate' value with any optional fields omitted.
mkDeleteHsmClientCertificate
    :: Core.Text -- ^ 'hsmClientCertificateIdentifier'
    -> DeleteHsmClientCertificate
mkDeleteHsmClientCertificate hsmClientCertificateIdentifier
  = DeleteHsmClientCertificate'{hsmClientCertificateIdentifier}

-- | The identifier of the HSM client certificate to be deleted.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHsmClientCertificateIdentifier :: Lens.Lens' DeleteHsmClientCertificate Core.Text
dHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE dHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

instance Core.ToQuery DeleteHsmClientCertificate where
        toQuery DeleteHsmClientCertificate{..}
          = Core.toQueryPair "Action"
              ("DeleteHsmClientCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "HsmClientCertificateIdentifier"
                hsmClientCertificateIdentifier

instance Core.ToHeaders DeleteHsmClientCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteHsmClientCertificate where
        type Rs DeleteHsmClientCertificate =
             DeleteHsmClientCertificateResponse
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
          = Response.receiveNull DeleteHsmClientCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteHsmClientCertificateResponse' smart constructor.
data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsmClientCertificateResponse' value with any optional fields omitted.
mkDeleteHsmClientCertificateResponse
    :: DeleteHsmClientCertificateResponse
mkDeleteHsmClientCertificateResponse
  = DeleteHsmClientCertificateResponse'
