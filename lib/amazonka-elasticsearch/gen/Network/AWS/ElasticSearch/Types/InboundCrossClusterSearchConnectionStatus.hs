{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
  ( InboundCrossClusterSearchConnectionStatus (..)
  -- * Smart constructor
  , mkInboundCrossClusterSearchConnectionStatus
  -- * Lenses
  , iccscsMessage
  , iccscsStatusCode
  ) where

import qualified Network.AWS.ElasticSearch.Types.CrossClusterSearchConnectionStatusMessage as Types
import qualified Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the coonection status of an inbound cross-cluster search connection.
--
-- /See:/ 'mkInboundCrossClusterSearchConnectionStatus' smart constructor.
data InboundCrossClusterSearchConnectionStatus = InboundCrossClusterSearchConnectionStatus'
  { message :: Core.Maybe Types.CrossClusterSearchConnectionStatusMessage
    -- ^ Specifies verbose information for the inbound connection status.
  , statusCode :: Core.Maybe Types.InboundCrossClusterSearchConnectionStatusCode
    -- ^ The state code for inbound connection. This can be one of the following:
--
--
--     * PENDING_ACCEPTANCE: Inbound connection is not yet accepted by destination domain owner.
--
--     * APPROVED: Inbound connection is pending acceptance by destination domain owner.
--
--     * REJECTING: Inbound connection rejection is in process.
--
--     * REJECTED: Inbound connection is rejected.
--
--     * DELETING: Inbound connection deletion is in progress.
--
--     * DELETED: Inbound connection is deleted and cannot be used further.
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InboundCrossClusterSearchConnectionStatus' value with any optional fields omitted.
mkInboundCrossClusterSearchConnectionStatus
    :: InboundCrossClusterSearchConnectionStatus
mkInboundCrossClusterSearchConnectionStatus
  = InboundCrossClusterSearchConnectionStatus'{message =
                                                 Core.Nothing,
                                               statusCode = Core.Nothing}

-- | Specifies verbose information for the inbound connection status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscsMessage :: Lens.Lens' InboundCrossClusterSearchConnectionStatus (Core.Maybe Types.CrossClusterSearchConnectionStatusMessage)
iccscsMessage = Lens.field @"message"
{-# INLINEABLE iccscsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The state code for inbound connection. This can be one of the following:
--
--
--     * PENDING_ACCEPTANCE: Inbound connection is not yet accepted by destination domain owner.
--
--     * APPROVED: Inbound connection is pending acceptance by destination domain owner.
--
--     * REJECTING: Inbound connection rejection is in process.
--
--     * REJECTED: Inbound connection is rejected.
--
--     * DELETING: Inbound connection deletion is in progress.
--
--     * DELETED: Inbound connection is deleted and cannot be used further.
--
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscsStatusCode :: Lens.Lens' InboundCrossClusterSearchConnectionStatus (Core.Maybe Types.InboundCrossClusterSearchConnectionStatusCode)
iccscsStatusCode = Lens.field @"statusCode"
{-# INLINEABLE iccscsStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.FromJSON InboundCrossClusterSearchConnectionStatus
         where
        parseJSON
          = Core.withObject "InboundCrossClusterSearchConnectionStatus"
              Core.$
              \ x ->
                InboundCrossClusterSearchConnectionStatus' Core.<$>
                  (x Core..:? "Message") Core.<*> x Core..:? "StatusCode"
