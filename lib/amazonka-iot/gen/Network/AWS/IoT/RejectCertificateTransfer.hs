{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RejectCertificateTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a pending certificate transfer. After AWS IoT rejects a certificate transfer, the certificate status changes from __PENDING_TRANSFER__ to __INACTIVE__ .
--
-- To check for pending certificate transfers, call 'ListCertificates' to enumerate your certificates.
-- This operation can only be called by the transfer destination. After it is called, the certificate will be returned to the source's account in the INACTIVE state.
module Network.AWS.IoT.RejectCertificateTransfer
    (
    -- * Creating a request
      RejectCertificateTransfer (..)
    , mkRejectCertificateTransfer
    -- ** Request lenses
    , rctCertificateId
    , rctRejectReason

    -- * Destructuring the response
    , RejectCertificateTransferResponse (..)
    , mkRejectCertificateTransferResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the RejectCertificateTransfer operation.
--
-- /See:/ 'mkRejectCertificateTransfer' smart constructor.
data RejectCertificateTransfer = RejectCertificateTransfer'
  { certificateId :: Types.CertificateId
    -- ^ The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
  , rejectReason :: Core.Maybe Types.RejectReason
    -- ^ The reason the certificate transfer was rejected.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectCertificateTransfer' value with any optional fields omitted.
mkRejectCertificateTransfer
    :: Types.CertificateId -- ^ 'certificateId'
    -> RejectCertificateTransfer
mkRejectCertificateTransfer certificateId
  = RejectCertificateTransfer'{certificateId,
                               rejectReason = Core.Nothing}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctCertificateId :: Lens.Lens' RejectCertificateTransfer Types.CertificateId
rctCertificateId = Lens.field @"certificateId"
{-# INLINEABLE rctCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The reason the certificate transfer was rejected.
--
-- /Note:/ Consider using 'rejectReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctRejectReason :: Lens.Lens' RejectCertificateTransfer (Core.Maybe Types.RejectReason)
rctRejectReason = Lens.field @"rejectReason"
{-# INLINEABLE rctRejectReason #-}
{-# DEPRECATED rejectReason "Use generic-lens or generic-optics with 'rejectReason' instead"  #-}

instance Core.ToQuery RejectCertificateTransfer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RejectCertificateTransfer where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RejectCertificateTransfer where
        toJSON RejectCertificateTransfer{..}
          = Core.object
              (Core.catMaybes [("rejectReason" Core..=) Core.<$> rejectReason])

instance Core.AWSRequest RejectCertificateTransfer where
        type Rs RejectCertificateTransfer =
             RejectCertificateTransferResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/reject-certificate-transfer/" Core.<> Core.toText certificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull RejectCertificateTransferResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectCertificateTransferResponse' smart constructor.
data RejectCertificateTransferResponse = RejectCertificateTransferResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectCertificateTransferResponse' value with any optional fields omitted.
mkRejectCertificateTransferResponse
    :: RejectCertificateTransferResponse
mkRejectCertificateTransferResponse
  = RejectCertificateTransferResponse'
