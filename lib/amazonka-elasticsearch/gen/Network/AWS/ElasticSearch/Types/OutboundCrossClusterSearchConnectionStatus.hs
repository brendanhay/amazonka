{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
  ( OutboundCrossClusterSearchConnectionStatus (..),

    -- * Smart constructor
    mkOutboundCrossClusterSearchConnectionStatus,

    -- * Lenses
    occscsMessage,
    occscsStatusCode,
  )
where

import qualified Network.AWS.ElasticSearch.Types.CrossClusterSearchConnectionStatusMessage as Types
import qualified Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the connection status of an outbound cross-cluster search connection.
--
-- /See:/ 'mkOutboundCrossClusterSearchConnectionStatus' smart constructor.
data OutboundCrossClusterSearchConnectionStatus = OutboundCrossClusterSearchConnectionStatus'
  { -- | Specifies verbose information for the outbound connection status.
    message :: Core.Maybe Types.CrossClusterSearchConnectionStatusMessage,
    -- | The state code for outbound connection. This can be one of the following:
    --
    --
    --     * VALIDATING: The outbound connection request is being validated.
    --
    --     * VALIDATION_FAILED: Validation failed for the connection request.
    --
    --     * PENDING_ACCEPTANCE: Outbound connection request is validated and is not yet accepted by destination domain owner.
    --
    --     * PROVISIONING: Outbound connection request is in process.
    --
    --     * ACTIVE: Outbound connection is active and ready to use.
    --
    --     * REJECTED: Outbound connection request is rejected by destination domain owner.
    --
    --     * DELETING: Outbound connection deletion is in progress.
    --
    --     * DELETED: Outbound connection is deleted and cannot be used further.
    statusCode :: Core.Maybe Types.OutboundCrossClusterSearchConnectionStatusCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutboundCrossClusterSearchConnectionStatus' value with any optional fields omitted.
mkOutboundCrossClusterSearchConnectionStatus ::
  OutboundCrossClusterSearchConnectionStatus
mkOutboundCrossClusterSearchConnectionStatus =
  OutboundCrossClusterSearchConnectionStatus'
    { message =
        Core.Nothing,
      statusCode = Core.Nothing
    }

-- | Specifies verbose information for the outbound connection status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscsMessage :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Core.Maybe Types.CrossClusterSearchConnectionStatusMessage)
occscsMessage = Lens.field @"message"
{-# DEPRECATED occscsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The state code for outbound connection. This can be one of the following:
--
--
--     * VALIDATING: The outbound connection request is being validated.
--
--     * VALIDATION_FAILED: Validation failed for the connection request.
--
--     * PENDING_ACCEPTANCE: Outbound connection request is validated and is not yet accepted by destination domain owner.
--
--     * PROVISIONING: Outbound connection request is in process.
--
--     * ACTIVE: Outbound connection is active and ready to use.
--
--     * REJECTED: Outbound connection request is rejected by destination domain owner.
--
--     * DELETING: Outbound connection deletion is in progress.
--
--     * DELETED: Outbound connection is deleted and cannot be used further.
--
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscsStatusCode :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Core.Maybe Types.OutboundCrossClusterSearchConnectionStatusCode)
occscsStatusCode = Lens.field @"statusCode"
{-# DEPRECATED occscsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Core.FromJSON OutboundCrossClusterSearchConnectionStatus where
  parseJSON =
    Core.withObject "OutboundCrossClusterSearchConnectionStatus" Core.$
      \x ->
        OutboundCrossClusterSearchConnectionStatus'
          Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "StatusCode")
